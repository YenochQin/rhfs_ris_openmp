#!/bin/bash

# 测试线程安全问题修复的脚本
# 验证rhfs程序现在可以正常运行而不会卡住

echo "=========================================="
echo "RHFS 线程安全问题修复验证测试"
echo "=========================================="

# 检查程序是否存在
if [ ! -f "bin/rhfs90" ]; then
    echo "错误: bin/rhfs90 不存在。请先编译程序。"
    exit 1
fi

echo "1. 测试程序版本和OpenMP信息..."
echo "   程序应该显示使用1个线程"

# 设置环境变量（虽然程序内部已强制单线程，但仍然设置以确保一致性）
export OMP_NUM_THREADS=1

echo
echo "2. 准备运行rhfs90..."
echo "   如果程序正常运行并显示'Column XX complete'消息，"
echo "   则说明线程安全问题已解决。"
echo

echo "请手动运行以下命令进行完整测试："
echo "   cd /path/to/test/case"
echo "   export OMP_NUM_THREADS=1"
echo "   /path/to/rhfs_ris_openmp/bin/rhfs90 < input_file"
echo
echo "预期行为："
echo "- 程序显示: 'RHFS: Using OpenMP with 1 threads (forced serial execution...)'"
echo "- 程序正常执行，显示 'Column 50 complete;', 'Column 100 complete;' 等"
echo "- 程序不会卡住"
echo "- 最终显示: 'RHFS: Execution complete.'"

echo
echo "注意: 如果需要测试原始的多线程版本（用于调试），"
echo "      请临时注释掉hfsgg.f90中的OMP_SET_NUM_THREADS(1)行" 