#!/bin/bash

# RHFS90 性能基准测试脚本
# 比较串行版本和不同线程数的并行版本性能

set -e

# 配置
INPUT_NAME=${1:-"test"}
MAX_THREADS=${2:-$(nproc)}
TEST_ITERATIONS=3

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}   RHFS90 性能基准测试${NC}"
echo -e "${BLUE}========================================${NC}"
echo

# 检查输入文件
if [ ! -f "${INPUT_NAME}.c" ] || [ ! -f "${INPUT_NAME}.w" ]; then
    echo -e "${RED}错误: 输入文件 ${INPUT_NAME}.c 或 ${INPUT_NAME}.w 不存在${NC}"
    exit 1
fi

# 创建结果目录
RESULT_DIR="benchmark_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$RESULT_DIR"

echo -e "${YELLOW}测试配置:${NC}"
echo "输入文件: $INPUT_NAME"
echo "最大线程数: $MAX_THREADS"
echo "测试迭代次数: $TEST_ITERATIONS"
echo "结果目录: $RESULT_DIR"
echo

# 编译串行和并行版本
echo -e "${YELLOW}编译程序...${NC}"
cd src

echo "编译串行版本..."
make clean > /dev/null 2>&1
make serial > /dev/null 2>&1
cp ../rhfs ../rhfs_serial

echo "编译并行版本..."
make clean > /dev/null 2>&1  
make parallel > /dev/null 2>&1
cp ../rhfs ../rhfs_parallel

cd ..

# 结果文件
RESULTS_FILE="$RESULT_DIR/benchmark_results.txt"
CSV_FILE="$RESULT_DIR/benchmark_results.csv"

# 创建CSV头部
echo "Version,Threads,Iteration,Time(s),Speedup" > "$CSV_FILE"

# 测试函数
run_test() {
    local version=$1
    local threads=$2
    local iteration=$3
    local exec_path=$4
    
    echo -n "  迭代 $iteration: "
    
    if [ "$version" = "serial" ]; then
        export OMP_NUM_THREADS=1
    else
        export OMP_NUM_THREADS=$threads
    fi
    
    START_TIME=$(date +%s.%N)
    {
        echo "$INPUT_NAME"
        echo "y"
        echo "y"
    } | timeout 3600 $exec_path > /dev/null 2>&1
    END_TIME=$(date +%s.%N)
    
    RUNTIME=$(echo "$END_TIME - $START_TIME" | bc)
    echo "${RUNTIME}s"
    
    echo "$RUNTIME"
}

# 存储串行基准时间
SERIAL_TIMES=()

echo -e "${GREEN}测试串行版本...${NC}"
for i in $(seq 1 $TEST_ITERATIONS); do
    time=$(run_test "serial" 1 $i "./rhfs_serial")
    SERIAL_TIMES+=($time)
done

# 计算串行版本平均时间
SERIAL_AVG=$(echo "${SERIAL_TIMES[@]}" | tr ' ' '\n' | awk '{sum+=$1} END {print sum/NR}')
echo "串行版本平均时间: ${SERIAL_AVG}s"
echo

# 写入结果文件头部
{
    echo "RHFS90 性能基准测试报告"
    echo "======================"
    echo
    echo "测试时间: $(date)"
    echo "输入文件: $INPUT_NAME"
    echo "测试迭代次数: $TEST_ITERATIONS"
    echo "系统信息:"
    echo "  CPU: $(lscpu | grep 'Model name' | cut -d: -f2 | xargs)"
    echo "  核心数: $(nproc)"
    echo "  内存: $(free -h | awk '/^Mem:/ {print $2}')"
    echo
    echo "结果:"
    echo "======"
    echo
} > "$RESULTS_FILE"

# 记录串行结果
{
    echo "串行版本:"
    printf "  时间: %.3fs (平均)\n" "$SERIAL_AVG"
    echo "  详细时间: ${SERIAL_TIMES[@]}"
    echo
} >> "$RESULTS_FILE"

# 添加串行结果到CSV
for i in $(seq 0 $((TEST_ITERATIONS-1))); do
    echo "serial,1,$((i+1)),${SERIAL_TIMES[$i]},1.00" >> "$CSV_FILE"
done

# 测试不同线程数的并行版本
echo -e "${GREEN}测试并行版本...${NC}"
for threads in $(seq 2 $MAX_THREADS); do
    echo "测试 $threads 线程:"
    
    PARALLEL_TIMES=()
    for i in $(seq 1 $TEST_ITERATIONS); do
        time=$(run_test "parallel" $threads $i "./rhfs_parallel")
        PARALLEL_TIMES+=($time)
    done
    
    # 计算平均时间和加速比
    PARALLEL_AVG=$(echo "${PARALLEL_TIMES[@]}" | tr ' ' '\n' | awk '{sum+=$1} END {print sum/NR}')
    SPEEDUP=$(echo "scale=2; $SERIAL_AVG / $PARALLEL_AVG" | bc)
    EFFICIENCY=$(echo "scale=1; $SPEEDUP * 100 / $threads" | bc)
    
    echo "  平均时间: ${PARALLEL_AVG}s"
    echo "  加速比: ${SPEEDUP}x"
    echo "  效率: ${EFFICIENCY}%"
    echo
    
    # 记录到结果文件
    {
        echo "${threads} 线程并行版本:"
        printf "  时间: %.3fs (平均)\n" "$PARALLEL_AVG"
        printf "  加速比: %.2fx\n" "$SPEEDUP"
        printf "  并行效率: %.1f%%\n" "$EFFICIENCY"
        echo "  详细时间: ${PARALLEL_TIMES[@]}"
        echo
    } >> "$RESULTS_FILE"
    
    # 添加到CSV
    for i in $(seq 0 $((TEST_ITERATIONS-1))); do
        speedup_i=$(echo "scale=2; $SERIAL_AVG / ${PARALLEL_TIMES[$i]}" | bc)
        echo "parallel,$threads,$((i+1)),${PARALLEL_TIMES[$i]},$speedup_i" >> "$CSV_FILE"
    done
done

# 生成性能图表数据
echo -e "${YELLOW}生成性能分析...${NC}"
{
    echo
    echo "性能总结:"
    echo "========"
    echo
    printf "%-8s %-12s %-10s %-10s\n" "线程数" "平均时间(s)" "加速比" "效率(%)"
    echo "----------------------------------------"
} >> "$RESULTS_FILE"

# 重新计算并显示汇总
BEST_SPEEDUP=1.0
BEST_THREADS=1

echo "串行     ${SERIAL_AVG}    1.00x     100.0%" >> "$RESULTS_FILE"

for threads in $(seq 2 $MAX_THREADS); do
    # 从CSV文件中计算该线程数的平均值
    avg_time=$(awk -F, -v t="$threads" '$1=="parallel" && $2==t {sum+=$4; count++} END {print sum/count}' "$CSV_FILE")
    speedup=$(echo "scale=2; $SERIAL_AVG / $avg_time" | bc)
    efficiency=$(echo "scale=1; $speedup * 100 / $threads" | bc)
    
    printf "%-8d %-12.3f %-10.2fx %-10.1f%%\n" "$threads" "$avg_time" "$speedup" "$efficiency" >> "$RESULTS_FILE"
    
    # 跟踪最佳性能
    if (( $(echo "$speedup > $BEST_SPEEDUP" | bc -l) )); then
        BEST_SPEEDUP=$speedup
        BEST_THREADS=$threads
    fi
done

{
    echo
    echo "最佳配置: $BEST_THREADS 线程 (加速比: ${BEST_SPEEDUP}x)"
    echo
    echo "建议:"
    if (( $(echo "$BEST_SPEEDUP < 1.5" | bc -l) )); then
        echo "- 并行化效果有限，可能受到I/O或内存带宽限制"
    elif (( $(echo "$BEST_SPEEDUP > $MAX_THREADS * 0.7" | bc -l) )); then
        echo "- 并行化效果良好，建议使用 $BEST_THREADS 线程"
    else
        echo "- 并行化效果中等，建议使用 $BEST_THREADS 线程"
    fi
    echo "- 对于生产环境，建议使用 $BEST_THREADS 线程进行计算"
} >> "$RESULTS_FILE"

# 清理临时文件
rm -f rhfs_serial rhfs_parallel
rm -f "${INPUT_NAME}".h "${INPUT_NAME}".hoffd "${INPUT_NAME}".sum "${INPUT_NAME}".dbg

echo -e "${GREEN}基准测试完成!${NC}"
echo
echo "结果文件:"
echo "  详细报告: $RESULTS_FILE"
echo "  CSV数据: $CSV_FILE"
echo
echo "最佳配置: $BEST_THREADS 线程 (加速比: ${BEST_SPEEDUP}x)"
echo
echo -e "${BLUE}查看完整报告: cat $RESULTS_FILE${NC}" 