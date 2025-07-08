#!/bin/bash
echo "=== CSF文件预处理为二进制格式 ==="
echo "注意: 这需要修改GRASP源码，较为复杂"
echo "建议优先使用方案1和方案2"

# 检查CSF文件大小
if [ -f "even1as2069as2.c" ]; then
    csf_size=$(stat -c%s "even1as2069as2.c")
    csf_size_mb=$((csf_size / 1024 / 1024))
    echo "CSF文件大小: ${csf_size_mb}MB"
    
    if [ $csf_size_mb -gt 1024 ]; then
        echo "大型CSF文件，强烈建议使用内存文件系统优化"
    fi
fi
