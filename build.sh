#!/bin/bash

# RHFS90 自动编译脚本
# 自动编译库文件和主程序

set -e  # 遇到错误时退出

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 解析命令行参数
CLEAN_BUILD=false
NO_OPENMP=false
VERBOSE=false
JOBS=1

while [[ $# -gt 0 ]]; do
    case $1 in
        --clean|-c)
            CLEAN_BUILD=true
            shift
            ;;
        --no-openmp)
            NO_OPENMP=true
            shift
            ;;
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        --jobs|-j)
            JOBS="$2"
            shift 2
            ;;
        --help|-h)
            echo "RHFS90 自动编译脚本"
            echo "=================="
            echo "用法: $0 [选项]"
            echo ""
            echo "选项:"
            echo "  --clean, -c      清理后重新编译"
            echo "  --no-openmp      禁用OpenMP支持"
            echo "  --verbose, -v    显示详细编译信息"
            echo "  --jobs, -j N     使用N个并行任务编译"
            echo "  --help, -h       显示此帮助信息"
            echo ""
            echo "示例:"
            echo "  $0                    # 基本编译"
            echo "  $0 --clean           # 清理后编译"
            echo "  $0 --no-openmp       # 编译串行版本"
            echo "  $0 --jobs 4          # 使用4个并行任务"
            exit 0
            ;;
        *)
            echo "未知选项: $1"
            echo "使用 --help 查看帮助信息"
            exit 1
            ;;
    esac
done

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}      RHFS90 自动编译脚本${NC}"
echo -e "${BLUE}========================================${NC}"
echo

# 显示编译配置
echo -e "${YELLOW}编译配置:${NC}"
echo "清理编译: $([ "$CLEAN_BUILD" = true ] && echo "是" || echo "否")"
echo "OpenMP支持: $([ "$NO_OPENMP" = true ] && echo "否" || echo "是")"
echo "并行任务数: $JOBS"
echo "详细输出: $([ "$VERBOSE" = true ] && echo "是" || echo "否")"
echo

# 设置Make选项
MAKE_OPTS=""
if [ "$NO_OPENMP" = true ]; then
    MAKE_OPTS="$MAKE_OPTS NO_OPENMP=1"
fi

if [ "$VERBOSE" = false ]; then
    MAKE_OPTS="$MAKE_OPTS -s"
fi

if [ "$JOBS" -gt 1 ]; then
    MAKE_OPTS="$MAKE_OPTS -j$JOBS"
fi

# 函数：执行Make命令
run_make() {
    local dir=$1
    local target=$2
    
    echo -e "${YELLOW}在 $dir 目录执行: make $target${NC}"
    if [ "$VERBOSE" = true ]; then
        (cd "$dir" && make $MAKE_OPTS $target)
    else
        (cd "$dir" && make $MAKE_OPTS $target >/dev/null 2>&1) || {
            echo -e "${RED}编译失败，显示详细错误信息:${NC}"
            (cd "$dir" && make $target)
            exit 1
        }
    fi
}

# 记录开始时间
START_TIME=$(date +%s)

# 清理（如果需要）
if [ "$CLEAN_BUILD" = true ]; then
    echo -e "${YELLOW}清理现有编译文件...${NC}"
    
    if [ -d "libs" ]; then
        run_make "libs" "clean"
    fi
    
    if [ -d "src" ]; then
        run_make "src" "clean"
    fi
    
    echo -e "${GREEN}清理完成!${NC}"
    echo
fi

# 检查目录结构
echo -e "${YELLOW}检查项目结构...${NC}"

if [ ! -d "libs" ]; then
    echo -e "${RED}错误: libs 目录不存在${NC}"
    exit 1
fi

if [ ! -d "src" ]; then
    echo -e "${RED}错误: src 目录不存在${NC}"
    exit 1
fi

if [ ! -f "libs/Makefile" ]; then
    echo -e "${RED}错误: libs/Makefile 不存在${NC}"
    exit 1
fi

if [ ! -f "src/Makefile" ]; then
    echo -e "${RED}错误: src/Makefile 不存在${NC}"
    exit 1
fi

echo -e "${GREEN}项目结构检查通过!${NC}"
echo

# 编译库文件
echo -e "${GREEN}第1步: 编译库文件${NC}"
echo "======================================="

# 显示库文件编译进度
if [ "$VERBOSE" = true ]; then
    run_make "libs" "progress"
    echo
fi

# 编译库文件
run_make "libs" "all"

# 验证库文件
echo -e "${YELLOW}验证库文件...${NC}"
if [ ! -f "libs/librhfs90.a" ]; then
    echo -e "${RED}错误: 库文件 libs/librhfs90.a 未生成${NC}"
    exit 1
fi

LIB_SIZE=$(du -h libs/librhfs90.a | cut -f1)
echo -e "${GREEN}库文件编译成功! (大小: $LIB_SIZE)${NC}"
echo

# 编译主程序
echo -e "${GREEN}第2步: 编译主程序${NC}"
echo "======================================="

run_make "src" "all"

# 验证可执行文件
echo -e "${YELLOW}验证可执行文件...${NC}"
if [ ! -f "rhfs" ]; then
    echo -e "${RED}错误: 可执行文件 rhfs 未生成${NC}"
    exit 1
fi

EXE_SIZE=$(du -h rhfs | cut -f1)
echo -e "${GREEN}主程序编译成功! (大小: $EXE_SIZE)${NC}"
echo

# 计算编译时间
END_TIME=$(date +%s)
COMPILE_TIME=$((END_TIME - START_TIME))
MINUTES=$((COMPILE_TIME / 60))
SECONDS=$((COMPILE_TIME % 60))

# 显示编译摘要
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}编译完成摘要${NC}"
echo -e "${BLUE}========================================${NC}"
echo -e "${GREEN}总编译时间: ${MINUTES}分${SECONDS}秒${NC}"
echo
echo "生成的文件:"
echo "  库文件: libs/librhfs90.a ($LIB_SIZE)"
echo "  可执行文件: rhfs ($EXE_SIZE)"
echo

# 显示使用建议
echo -e "${YELLOW}使用建议:${NC}"
if [ "$NO_OPENMP" = false ]; then
    echo "  设置线程数: export OMP_NUM_THREADS=4"
    echo "  运行程序: ./rhfs"
    echo "  或使用脚本: ./run_parallel.sh 4 your_input_name"
else
    echo "  运行串行版本: ./rhfs"
fi

echo "  性能测试: ./benchmark.sh your_input_name"
echo "  查看帮助: cd src && make help"
echo

echo -e "${GREEN}编译成功完成!${NC}" 