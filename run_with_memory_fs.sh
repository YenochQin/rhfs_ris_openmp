#!/bin/bash
echo "=== 创建内存文件系统并运行 RHFS ==="

# 计算合理的内存分配
total_mem_kb=$(awk '/MemTotal/ {print $2}' /proc/meminfo)
tmpfs_size=$((total_mem_kb / 1024 / 8))  # 使用1/8的内存
if [ $tmpfs_size -lt 1024 ]; then
    tmpfs_size=1024  # 最小1GB
fi
if [ $tmpfs_size -gt 32768 ]; then
    tmpfs_size=32768  # 最大32GB
fi

echo "为tmpfs分配 ${tmpfs_size}MB 内存"

# 创建内存文件系统
work_dir="/tmp/grasp_memory_work_$$"
echo "创建工作目录: $work_dir"
sudo mkdir -p "$work_dir"
sudo mount -t tmpfs -o size=${tmpfs_size}M tmpfs "$work_dir"
sudo chown $USER:$USER "$work_dir"

# 检查需要的文件
required_files=("even1as2069as2.c" "even1as2069as2.w" "even1as2069as2.cm" "isodata")
missing_files=()

for file in "${required_files[@]}"; do
    if [ ! -f "$file" ]; then
        echo "警告: 找不到文件 $file"
        missing_files+=("$file")
    fi
done

if [ ${#missing_files[@]} -gt 0 ]; then
    echo "错误: 缺少必需文件，请确保在正确目录运行"
    echo "缺少的文件: ${missing_files[*]}"
    sudo umount "$work_dir"
    sudo rmdir "$work_dir"
    exit 1
fi

# 复制文件到内存
echo "复制文件到内存文件系统..."
cp even1as2069as2.* isodata "$work_dir/"

# 显示内存使用
echo "文件大小:"
ls -lh "$work_dir"

# 进入内存目录并运行
echo ""
echo "=== 在内存中运行 RHFS ==="
cd "$work_dir"

# 设置优化的环境变量
export OMP_NUM_THREADS=46
export OMP_PLACES=cores
export OMP_PROC_BIND=close
export MALLOC_ARENA_MAX=4

# 运行程序并计时
time rhfs_omp << 'INPUT'
y
even1as2069as2
y
INPUT

echo ""
echo "=== 运行完成，复制结果文件 ==="
# 复制结果回原目录
cp even1as2069as2.ch even1as2069as2.choffd "$OLDPWD/" 2>/dev/null
cd "$OLDPWD"

# 清理内存文件系统
echo "清理内存文件系统..."
sudo umount "$work_dir"
sudo rmdir "$work_dir"

echo "优化运行完成！"
