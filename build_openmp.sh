#!/bin/bash

# GRASP程序包 OpenMP 并行构建脚本
# 包含 RHFS90、RHFSZEEMAN95、RIS4 等程序
# 使用 CMake 构建支持 OpenMP 的版本

set -e  # 遇到错误时退出

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 解析命令行参数
CLEAN_BUILD=false
BUILD_TYPE="Release"
VERBOSE=false
JOBS=$(nproc)
NO_OPENMP=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --clean|-c)
            CLEAN_BUILD=true
            shift
            ;;
        --debug|-d)
            BUILD_TYPE="Debug"
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
            echo "GRASP程序包 OpenMP 并行构建脚本"
            echo "============================"
            echo "用法: $0 [选项]"
            echo ""
            echo "选项:"
            echo "  --clean, -c      清理后重新编译"
            echo "  --debug, -d      Debug 构建（默认为 Release）"
            echo "  --no-openmp      禁用 OpenMP 支持"
            echo "  --verbose, -v    显示详细编译信息"
            echo "  --jobs, -j N     使用 N 个并行任务编译（默认为 CPU 核心数）"
            echo "  --help, -h       显示此帮助信息"
            echo ""
            echo "示例:"
            echo "  $0                    # 基本 OpenMP 并行编译"
            echo "  $0 --clean           # 清理后编译"
            echo "  $0 --debug           # Debug 模式编译"
            echo "  $0 --no-openmp       # 编译串行版本"
            echo "  $0 --jobs 8          # 使用8个并行任务"
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
echo -e "${BLUE}     GRASP程序包 OpenMP 并行构建脚本${NC}"
echo -e "${BLUE}========================================${NC}"
echo

# 显示编译配置
echo -e "${YELLOW}编译配置:${NC}"
echo "构建类型: $BUILD_TYPE"
echo "清理构建: $([ "$CLEAN_BUILD" = true ] && echo "是" || echo "否")"
echo "OpenMP支持: $([ "$NO_OPENMP" = true ] && echo "否" || echo "是")"
echo "并行任务数: $JOBS"
echo "详细输出: $([ "$VERBOSE" = true ] && echo "是" || echo "否")"
echo

# 检查是否需要加载 MPI 模块
echo -e "${YELLOW}检查 MPI 环境...${NC}"
if ! command -v mpifort &> /dev/null; then
    echo -e "${YELLOW}警告: 未找到 mpifort，尝试加载 MPI 模块...${NC}"
    if command -v module &> /dev/null; then
        module load mpi/openmpi-x86_64 || echo -e "${YELLOW}警告: 无法加载 MPI 模块${NC}"
    fi
fi

# 记录开始时间
START_TIME=$(date +%s)

# 设置构建目录
if [ "$BUILD_TYPE" = "Debug" ]; then
    BUILD_DIR="build-debug"
else
    BUILD_DIR="build"
fi

if [ "$NO_OPENMP" = true ]; then
    BUILD_DIR="${BUILD_DIR}-serial"
fi

# 清理（如果需要）
if [ "$CLEAN_BUILD" = true ]; then
    echo -e "${YELLOW}清理现有构建目录...${NC}"
    rm -rf "$BUILD_DIR"
    echo -e "${GREEN}清理完成!${NC}"
    echo
fi

# 创建构建目录
if [ ! -d "$BUILD_DIR" ]; then
    echo -e "${YELLOW}创建构建目录: $BUILD_DIR${NC}"
    mkdir -p "$BUILD_DIR"
fi

cd "$BUILD_DIR"

# 运行 CMake 配置
echo -e "${GREEN}第1步: CMake 配置${NC}"
echo "======================================="

CMAKE_ARGS="-DCMAKE_BUILD_TYPE=$BUILD_TYPE"

if [ "$NO_OPENMP" = true ]; then
    CMAKE_ARGS="$CMAKE_ARGS -DOpenMP_Fortran_FOUND=FALSE"
fi

if [ "$VERBOSE" = true ]; then
    echo "运行: cmake $CMAKE_ARGS .."
    cmake $CMAKE_ARGS ..
else
    cmake $CMAKE_ARGS .. > cmake_output.log 2>&1 || {
        echo -e "${RED}CMake 配置失败，显示日志:${NC}"
        cat cmake_output.log
        exit 1
    }
fi

echo -e "${GREEN}CMake 配置完成!${NC}"
echo

# 编译项目
echo -e "${GREEN}第2步: 编译项目${NC}"
echo "======================================="

MAKE_ARGS="-j$JOBS"
if [ "$VERBOSE" = false ]; then
    MAKE_ARGS="$MAKE_ARGS --silent"
fi

if [ "$VERBOSE" = true ]; then
    make $MAKE_ARGS
else
    make $MAKE_ARGS > make_output.log 2>&1 || {
        echo -e "${RED}编译失败，显示详细错误信息:${NC}"
        cat make_output.log
        exit 1
    }
fi

echo -e "${GREEN}编译完成!${NC}"
echo

# 安装可执行文件
echo -e "${GREEN}第3步: 安装可执行文件${NC}"
echo "======================================="

if [ "$VERBOSE" = true ]; then
    make install
else
    make install > install_output.log 2>&1 || {
        echo -e "${RED}安装失败，显示错误信息:${NC}"
        cat install_output.log
        exit 1
    }
fi

cd ..

# 验证可执行文件
echo -e "${YELLOW}验证可执行文件...${NC}"

MISSING_EXECUTABLES=()

if [ ! -f "bin/rhfs" ]; then
    MISSING_EXECUTABLES+=("bin/rhfs")
fi

if [ ! -f "bin/rhfszeeman95" ]; then
    MISSING_EXECUTABLES+=("bin/rhfszeeman95")
fi

if [ ! -f "bin/ris4" ]; then
    MISSING_EXECUTABLES+=("bin/ris4")
fi

if [ ${#MISSING_EXECUTABLES[@]} -gt 0 ]; then
    echo -e "${RED}错误: 以下可执行文件未生成:${NC}"
    for exe in "${MISSING_EXECUTABLES[@]}"; do
        echo -e "${RED}  - $exe${NC}"
    done
    exit 1
fi

# 显示生成的可执行文件信息
echo -e "${GREEN}所有可执行文件生成成功!${NC}"
echo "生成的可执行文件:"
for exe in bin/rhfs bin/rhfszeeman95 bin/ris4; do
    if [ -f "$exe" ]; then
        EXE_SIZE=$(du -h "$exe" | cut -f1)
        echo "  - $exe ($EXE_SIZE)"
    fi
done
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
echo "构建目录: $BUILD_DIR"
echo "构建类型: $BUILD_TYPE"
echo "OpenMP支持: $([ "$NO_OPENMP" = true ] && echo "否" || echo "是")"
echo
echo "生成的文件:"
for exe in bin/rhfs bin/rhfszeeman95 bin/ris4; do
    if [ -f "$exe" ]; then
        EXE_SIZE=$(du -h "$exe" | cut -f1)
        echo "  - $exe ($EXE_SIZE)"
    fi
done
echo

# 显示使用建议
echo -e "${YELLOW}使用建议:${NC}"
if [ "$NO_OPENMP" = false ]; then
    echo "  设置线程数: export OMP_NUM_THREADS=4"
    echo "  运行程序:"
    echo "    RHFS90 (超精细结构): ./bin/rhfs"
    echo "    RHFSZEEMAN95 (超精细和塞曼效应): ./bin/rhfszeeman95"  
    echo "    RIS4 (同位素位移): ./bin/ris4"
    echo "  或使用脚本: ./run_parallel.sh 4 your_input_name"
else
    echo "  运行串行版本:"
    echo "    RHFS90: ./bin/rhfs"
    echo "    RHFSZEEMAN95: ./bin/rhfszeeman95"
    echo "    RIS4: ./bin/ris4"
fi

echo "  查看帮助: ./bin/rhfs --help (或其他程序的 --help)"
echo

echo -e "${GREEN}GRASP程序包 OpenMP 并行编译成功完成!${NC}" 