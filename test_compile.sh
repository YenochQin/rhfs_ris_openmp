#!/bin/bash
# 快速编译测试脚本 - 验证优化后的代码编译正常

echo "开始编译测试..."

cd src/rhfs90
echo "编译 RHFS模块..."
if gfortran -fopenmp -O2 -c hfsgg.f90 2>/dev/null; then
    echo "✓ hfsgg.f90 编译成功"
else
    echo "✗ hfsgg.f90 编译失败"
fi

cd ../ris4
echo "编译 RIS模块..."
if gfortran -fopenmp -O2 -c rintdensvec.f90 2>/dev/null; then
    echo "✓ rintdensvec.f90 编译成功"
else
    echo "✗ rintdensvec.f90 编译失败"
fi

if gfortran -fopenmp -O2 -c getmixblock.f90 2>/dev/null; then
    echo "✓ getmixblock.f90 编译成功"
else
    echo "✗ getmixblock.f90 编译失败"
fi

if gfortran -fopenmp -O2 -c ris_cal.f90 2>/dev/null; then
    echo "✓ ris_cal.f90 编译成功"
else
    echo "✗ ris_cal.f90 编译失败"
fi

# 如果有fical.f90则编译（这是一个程序）
if [ -f fical.f90 ]; then
    if gfortran -O2 -c fical.f90 2>/dev/null; then
        echo "✓ fical.f90 编译成功"
    else
        echo "✗ fical.f90 编译失败"
    fi
fi

echo "编译测试完成" 