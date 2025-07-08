#!/bin/bash
echo "=== 应用系统级内存优化 ==="

# 1. 设置大页内存
echo "配置大页内存..."
current_hugepages=$(cat /proc/sys/vm/nr_hugepages)
echo "当前大页数: $current_hugepages"

# 计算合理的大页数 (每个大页2MB)
total_mem_mb=$(free -m | awk '/^Mem:/{print $2}')
suggested_hugepages=$((total_mem_mb / 8))  # 使用1/8内存作为大页
if [ $suggested_hugepages -gt 16384 ]; then
    suggested_hugepages=16384  # 最大32GB大页
fi

echo "建议设置大页数: $suggested_hugepages (约 $((suggested_hugepages * 2))MB)"
echo "sudo echo $suggested_hugepages > /proc/sys/vm/nr_hugepages"

# 2. 优化内存管理
echo ""
echo "内存管理优化设置:"
echo "export MALLOC_ARENA_MAX=4"
echo "export MALLOC_MMAP_THRESHOLD_=65536"
echo "export GFORTRAN_FORMATTED_BUFFER_SIZE=64"

# 3. OpenMP优化
echo ""
echo "OpenMP优化设置:"
echo "export OMP_PLACES=cores"
echo "export OMP_PROC_BIND=close"
echo "export OMP_STACKSIZE=64M"

# 4. 文件系统缓存优化
echo ""
echo "文件系统缓存优化:"
echo "sudo sysctl -w vm.dirty_ratio=15"
echo "sudo sysctl -w vm.dirty_background_ratio=5"
echo "sudo sysctl -w vm.vfs_cache_pressure=50"

echo ""
echo "要应用这些设置，请运行:"
echo "source optimize_system_memory.sh"
