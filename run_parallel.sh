#!/bin/bash

# GRASP程序包 OpenMP并行运行脚本
# 支持RHFS90、RHFSZEEMAN95、RIS4程序
# 使用方法: ./run_parallel.sh [程序名] [线程数] [输入文件名]

set -e  # 遇到错误时退出

# 默认设置
DEFAULT_PROGRAM="rhfs"
DEFAULT_THREADS=4
DEFAULT_NAME="test"

# 解析命令行参数
if [[ $1 =~ ^(rhfs|rhfszeeman95|ris4)$ ]]; then
    PROGRAM=$1
    THREADS=${2:-$DEFAULT_THREADS}
    INPUT_NAME=${3:-$DEFAULT_NAME}
elif [[ $1 =~ ^[0-9]+$ ]]; then
    # 向后兼容: 如果第一个参数是数字，则默认运行rhfs
    PROGRAM=$DEFAULT_PROGRAM
    THREADS=$1
    INPUT_NAME=${2:-$DEFAULT_NAME}
else
    PROGRAM=${1:-$DEFAULT_PROGRAM}
    THREADS=${2:-$DEFAULT_THREADS}
    INPUT_NAME=${3:-$DEFAULT_NAME}
fi

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 显示帮助信息
if [[ $1 == "--help" || $1 == "-h" ]]; then
    echo "GRASP程序包 OpenMP并行运行脚本"
    echo "================================"
    echo "用法: $0 [程序名] [线程数] [输入文件名]"
    echo "或:   $0 [线程数] [输入文件名]  # 默认运行rhfs"
    echo ""
    echo "支持的程序:"
    echo "  rhfs         - RHFS90 (超精细结构计算)"
    echo "  rhfszeeman95 - RHFSZEEMAN95 (超精细和塞曼效应)"
    echo "  ris4         - RIS4 (同位素位移计算)"
    echo ""
    echo "示例:"
    echo "  $0 rhfs 4 my_atom"
    echo "  $0 rhfszeeman95 8 my_atom"
    echo "  $0 ris4 2 my_atom"
    echo "  $0 4 my_atom    # 使用4线程运行rhfs"
    exit 0
fi

# 程序信息映射
case $PROGRAM in
    "rhfs")
        PROGRAM_NAME="RHFS90"
        PROGRAM_DESC="超精细结构计算"
        EXEC_PATH="bin/rhfs"
        ;;
    "rhfszeeman95")
        PROGRAM_NAME="RHFSZEEMAN95"
        PROGRAM_DESC="超精细和塞曼效应计算"
        EXEC_PATH="bin/rhfszeeman95"
        ;;
    "ris4")
        PROGRAM_NAME="RIS4"
        PROGRAM_DESC="同位素位移计算"
        EXEC_PATH="bin/ris4"
        ;;
    *)
        echo -e "${RED}错误: 不支持的程序 '$PROGRAM'${NC}"
        echo "支持的程序: rhfs, rhfszeeman95, ris4"
        echo "使用 --help 查看详细帮助"
        exit 1
        ;;
esac

# 显示标题
echo -e "${BLUE}============================================${NC}"
echo -e "${BLUE}   $PROGRAM_NAME OpenMP 并行化版本运行脚本${NC}"
echo -e "${BLUE}   $PROGRAM_DESC${NC}"
echo -e "${BLUE}============================================${NC}"
echo

# 检查系统信息
echo -e "${YELLOW}系统信息:${NC}"
echo "CPU核心数: $(nproc)"
echo "可用内存: $(free -h | awk '/^Mem:/ {print $7}')"
echo "操作系统: $(uname -s)"
echo

# 设置OpenMP环境变量
export OMP_NUM_THREADS=$THREADS
export OMP_PROC_BIND=true
export OMP_PLACES=cores
export OMP_DYNAMIC=false

echo -e "${YELLOW}OpenMP配置:${NC}"
echo "线程数: $OMP_NUM_THREADS"
echo "处理器绑定: $OMP_PROC_BIND"
echo "内存位置: $OMP_PLACES"
echo

# 检查是否存在可执行文件
if [ ! -f "$EXEC_PATH" ]; then
    echo -e "${RED}错误: 可执行文件 $EXEC_PATH 不存在${NC}"
    echo -e "${YELLOW}尝试使用CMake编译程序...${NC}"
    
    # 检查是否存在构建脚本
    if [ -f "build_openmp.sh" ]; then
        echo "使用OpenMP构建脚本..."
        ./build_openmp.sh
    else
        echo "使用基本CMake构建..."
        mkdir -p build
        cd build
        cmake ..
        make -j$(nproc)
        make install
        cd ..
    fi
    
    if [ ! -f "$EXEC_PATH" ]; then
        echo -e "${RED}编译失败! 请检查构建环境${NC}"
        exit 1
    fi
    echo -e "${GREEN}编译成功!${NC}"
fi

# 检查输入文件
REQUIRED_FILES=("${INPUT_NAME}.c" "${INPUT_NAME}.w" "isodata")
MISSING_FILES=()

for file in "${REQUIRED_FILES[@]}"; do
    if [ ! -f "$file" ]; then
        MISSING_FILES+=("$file")
    fi
done

if [ ${#MISSING_FILES[@]} -ne 0 ]; then
    echo -e "${RED}警告: 以下输入文件不存在:${NC}"
    for file in "${MISSING_FILES[@]}"; do
        echo "  - $file"
    done
    echo
    echo -e "${YELLOW}请确保输入文件存在或按Ctrl+C退出${NC}"
    read -p "按Enter继续..."
fi

# 创建输出目录
OUTPUT_DIR="output_${INPUT_NAME}_${THREADS}threads_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$OUTPUT_DIR"

echo -e "${YELLOW}运行配置:${NC}"
echo "输入文件名: $INPUT_NAME"
echo "输出目录: $OUTPUT_DIR"
echo "日志文件: $OUTPUT_DIR/${PROGRAM,,}.log"
echo

# 开始计时
START_TIME=$(date +%s)

echo -e "${GREEN}开始运行$PROGRAM_NAME...${NC}"
echo "==============================================="

# 运行程序并记录输出
{
    echo "$INPUT_NAME"
    echo "y"  # 默认设置
    echo "y"  # CI计算
} | $EXEC_PATH 2>&1 | tee "$OUTPUT_DIR/${PROGRAM,,}.log"

# 计算运行时间
END_TIME=$(date +%s)
RUNTIME=$((END_TIME - START_TIME))
MINUTES=$((RUNTIME / 60))
SECONDS=$((RUNTIME % 60))

echo "==============================================="
echo -e "${GREEN}运行完成!${NC}"
echo -e "${YELLOW}总运行时间: ${MINUTES}分${SECONDS}秒${NC}"

# 移动输出文件到输出目录
echo -e "${YELLOW}整理输出文件...${NC}"

# 根据不同程序移动相应的输出文件
case $PROGRAM in
    "rhfs")
        OUTPUT_EXTENSIONS=("h" "hoffd" "sum" "dbg")
        ;;
    "rhfszeeman95")
        OUTPUT_EXTENSIONS=("h" "gjhfs" "sum" "dbg")
        ;;
    "ris4")
        OUTPUT_EXTENSIONS=("i" "sum" "dbg")
        ;;
esac

for ext in "${OUTPUT_EXTENSIONS[@]}"; do
    if [ -f "${INPUT_NAME}.${ext}" ]; then
        mv "${INPUT_NAME}.${ext}" "$OUTPUT_DIR/"
        echo "已移动: ${INPUT_NAME}.${ext} -> $OUTPUT_DIR/"
    fi
done

# 移动可能的其他输出文件
for file in *.IOB *.ITB; do
    if [ -f "$file" ]; then
        mv "$file" "$OUTPUT_DIR/"
        echo "已移动: $file -> $OUTPUT_DIR/"
    fi
done

# 生成性能报告
echo -e "${YELLOW}生成性能报告...${NC}"
cat > "$OUTPUT_DIR/performance_report.txt" << EOF
$PROGRAM_NAME OpenMP 并行运行报告
=============================

运行时间: $(date)
输入文件: $INPUT_NAME
线程数: $THREADS
总运行时间: ${MINUTES}分${SECONDS}秒

系统配置:
- CPU核心数: $(nproc)
- 可用内存: $(free -h | awk '/^Mem:/ {print $2}')
- 操作系统: $(uname -a)

OpenMP配置:
- OMP_NUM_THREADS: $OMP_NUM_THREADS
- OMP_PROC_BIND: $OMP_PROC_BIND
- OMP_PLACES: $OMP_PLACES

输出文件:
$(ls -la "$OUTPUT_DIR"/ | grep -v "^total")

EOF

echo -e "${GREEN}性能报告已保存到: $OUTPUT_DIR/performance_report.txt${NC}"

# 显示结果摘要
echo
echo -e "${BLUE}============================================${NC}"
echo -e "${BLUE}运行完成摘要:${NC}"
echo -e "${BLUE}============================================${NC}"
echo "程序: $PROGRAM_NAME"
echo "运行时间: ${MINUTES}分${SECONDS}秒"
echo "输出目录: $OUTPUT_DIR"
echo "主要输出文件:"

# 显示相应程序的主要输出文件
case $PROGRAM in
    "rhfs")
        MAIN_EXTENSIONS=("h" "hoffd" "sum")
        ;;
    "rhfszeeman95")
        MAIN_EXTENSIONS=("h" "gjhfs" "sum")
        ;;
    "ris4")
        MAIN_EXTENSIONS=("i" "sum")
        ;;
esac

for ext in "${MAIN_EXTENSIONS[@]}"; do
    if [ -f "$OUTPUT_DIR/${INPUT_NAME}.${ext}" ]; then
        size=$(du -h "$OUTPUT_DIR/${INPUT_NAME}.${ext}" | cut -f1)
        echo "  - ${INPUT_NAME}.${ext} (${size})"
    fi
done

echo
echo -e "${GREEN}查看结果: ls -la $OUTPUT_DIR/${NC}"
echo -e "${GREEN}查看日志: cat $OUTPUT_DIR/${PROGRAM,,}.log${NC}"
echo 