#!/bin/bash

# RHFS OpenMP 并行化性能基准测试
# 测试不同线程数的性能，验证线程安全解决方案的效果

echo "=========================================="
echo "RHFS OpenMP 并行化性能基准测试"
echo "=========================================="

# 检查参数
if [ $# -lt 1 ]; then
    echo "用法: $0 <input_file> [max_threads]"
    echo "示例: $0 input.dat 8"
    exit 1
fi

INPUT_FILE="$1"
MAX_THREADS="${2:-$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo "4")}"

# 检查输入文件
if [ ! -f "$INPUT_FILE" ]; then
    echo "错误: 输入文件 $INPUT_FILE 不存在"
    exit 1
fi

# 检查程序
RHFS_PROG="./bin/rhfs90"
if [ ! -f "$RHFS_PROG" ]; then
    echo "错误: $RHFS_PROG 不存在，请先编译程序"
    exit 1
fi

echo "输入文件: $INPUT_FILE"
echo "最大线程数: $MAX_THREADS"
echo "程序: $RHFS_PROG"
echo

# 创建结果文件
RESULT_FILE="benchmark_results_$(date +%Y%m%d_%H%M%S).txt"
echo "结果将保存到: $RESULT_FILE"
echo

# 测试函数
run_benchmark() {
    local threads=$1
    echo "测试 $threads 线程..."
    
    export OMP_NUM_THREADS=$threads
    
    # 运行程序并测量时间
    local start_time=$(date +%s.%N)
    $RHFS_PROG < "$INPUT_FILE" > "output_${threads}threads.log" 2>&1
    local end_time=$(date +%s.%N)
    
    # 计算执行时间
    local execution_time=$(echo "$end_time - $start_time" | bc)
    
    # 检查是否成功完成
    if grep -q "Execution complete" "output_${threads}threads.log"; then
        echo "  ✅ 成功完成，耗时: ${execution_time}s"
        echo "$threads $execution_time" >> "$RESULT_FILE"
        return 0
    else
        echo "  ❌ 执行失败或未完成"
        echo "$threads FAILED" >> "$RESULT_FILE"
        return 1
    fi
}

# 开始基准测试
echo "开始基准测试..."
echo "=========================================="

# 写入结果文件头
echo "# RHFS OpenMP 基准测试结果" > "$RESULT_FILE"
echo "# 日期: $(date)" >> "$RESULT_FILE"
echo "# 输入文件: $INPUT_FILE" >> "$RESULT_FILE"
echo "# 格式: 线程数 执行时间(秒)" >> "$RESULT_FILE"
echo "# ================================" >> "$RESULT_FILE"

# 测试 1 线程（基准）
echo "1. 单线程基准测试"
if run_benchmark 1; then
    BASELINE_TIME=$(tail -n 1 "$RESULT_FILE" | cut -d' ' -f2)
    echo "   基准时间: ${BASELINE_TIME}s"
else
    echo "   基准测试失败，终止测试"
    exit 1
fi

echo

# 测试多线程
echo "2. 多线程性能测试"
for threads in 2 4 8 16; do
    if [ $threads -le $MAX_THREADS ]; then
        if run_benchmark $threads; then
            CURRENT_TIME=$(tail -n 1 "$RESULT_FILE" | cut -d' ' -f2)
            if [ "$CURRENT_TIME" != "FAILED" ]; then
                SPEEDUP=$(echo "scale=2; $BASELINE_TIME / $CURRENT_TIME" | bc)
                EFFICIENCY=$(echo "scale=1; $SPEEDUP / $threads * 100" | bc)
                echo "   加速比: ${SPEEDUP}x, 效率: ${EFFICIENCY}%"
            fi
        fi
    else
        echo "跳过 $threads 线程（超过最大线程数 $MAX_THREADS）"
    fi
    echo
done

echo "=========================================="
echo "基准测试完成！"
echo

# 生成报告
echo "性能报告:"
echo "========================================"
printf "%-8s %-12s %-10s %-10s\n" "线程数" "执行时间(s)" "加速比" "效率(%)"
echo "----------------------------------------"

while read -r line; do
    if [[ ! $line =~ ^# ]]; then
        threads=$(echo $line | cut -d' ' -f1)
        time=$(echo $line | cut -d' ' -f2)
        
        if [ "$time" != "FAILED" ]; then
            if [ "$threads" = "1" ]; then
                printf "%-8s %-12s %-10s %-10s\n" "$threads" "$time" "1.00" "100.0"
            else
                speedup=$(echo "scale=2; $BASELINE_TIME / $time" | bc)
                efficiency=$(echo "scale=1; $speedup / $threads * 100" | bc)
                printf "%-8s %-12s %-10s %-10s\n" "$threads" "$time" "$speedup" "$efficiency"
            fi
        else
            printf "%-8s %-12s %-10s %-10s\n" "$threads" "FAILED" "-" "-"
        fi
    fi
done < "$RESULT_FILE"

echo
echo "详细结果保存在: $RESULT_FILE"
echo "各线程的输出日志: output_*threads.log"

# 清理验证
echo
echo "验证结果一致性..."
if [ -f "output_1threads.log" ]; then
    reference_output="output_1threads.log"
    all_consistent=true
    
    for threads in 2 4 8 16; do
        if [ -f "output_${threads}threads.log" ] && [ $threads -le $MAX_THREADS ]; then
            # 简单检查：比较最终结果的关键数值
            if ! diff -q <(grep -E "HFS|TOTAL|Final" "$reference_output" 2>/dev/null || echo "") \
                       <(grep -E "HFS|TOTAL|Final" "output_${threads}threads.log" 2>/dev/null || echo "") >/dev/null 2>&1; then
                echo "⚠️  $threads 线程的结果与基准可能不同"
                all_consistent=false
            fi
        fi
    done
    
    if $all_consistent; then
        echo "✅ 所有结果与单线程基准一致"
    else
        echo "⚠️  建议手动检查输出文件的差异"
    fi
fi

echo
echo "基准测试总结："
echo "- 使用THREADPRIVATE实现的线程安全并行化"
echo "- 理想情况下效率应 > 80%"
echo "- 如果加速比不理想，可能受限于内存带宽或算法复杂度" 