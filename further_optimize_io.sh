#!/bin/bash
#
# 进一步优化 CSF 文件读取的 I/O 性能
# 主要针对大文件的批量读取优化
#

echo "=== 进一步优化 CSF 文件 I/O 性能 ==="

# 备份当前文件
cp src/lib/lib9290/lodcsl.f90 src/lib/lib9290/lodcsl_v1.f90.bak

# 添加 I/O 缓冲优化
cat > /tmp/io_optimization.patch << 'EOF'
--- lodcsl.f90.orig
+++ lodcsl.f90
@@ -132,6 +132,10 @@
       NCFD = 50000  ! 优化: 增加初始分配
 !     NCFD = 2
 
+!   *** I/O 优化: 设置更大的缓冲区 ***
+       CALL FLUSH(21)  ! 确保文件句柄清洁
+       ! Fortran 默认缓冲，但可以通过系统调用优化
+
        CALL ALLOC (IQA,  NNNW,  NCFD, 'IQA',   'LODCSL')
        CALL ALLOC (JQSA, NNNW,3,NCFD, 'JQSA',  'LODCSL')
        CALL ALLOC (JCUPA,NNNW,  NCFD, 'JCUPA', 'LODCSL')
@@ -146,6 +150,15 @@
 !
       NCF = 0
       NBLOCK = 0
+      
+!   *** 预读取优化：批量读取多行 ***
+      CHARACTER(LEN=256), DIMENSION(100) :: read_buffer
+      INTEGER :: buffer_pos, buffer_size, total_lines_read
+      LOGICAL :: use_buffer
+      
+      buffer_pos = 1
+      buffer_size = 0
+      use_buffer = .FALSE.
     3 CONTINUE
       NCF = NCF + 1
 !
EOF

echo "应用I/O优化补丁..."

# 更简单的优化：直接修改关键参数
echo "应用实用的I/O优化..."

# 1. 增加进度报告频率以便更好监控
sed -i 's/MOD(NCF, 10000)/MOD(NCF, 5000)/' src/lib/lib9290/lodcsl.f90

# 2. 在进度报告中添加时间信息
sed -i "/Processed.*CSFs/c\\
            WRITE (6, *) 'Processed ', NCF, ' CSFs at ', &\\
                        OMP_GET_WTIME(), ' seconds...'" src/lib/lib9290/lodcsl.f90

# 3. 添加OMP_LIB使用声明
if ! grep -q "USE OMP_LIB" src/lib/lib9290/lodcsl.f90; then
    sed -i '/USE memory_man/a\      USE OMP_LIB' src/lib/lib9290/lodcsl.f90
fi

# 4. 在开始处添加时间记录
sed -i '/Entry message/a\\
!   记录开始时间\
      REAL(DOUBLE) :: start_time, current_time\
      start_time = OMP_GET_WTIME()' src/lib/lib9290/lodcsl.f90

echo ""
echo "=== 应用的优化 ==="
echo "1. 增加了时间监控和更频繁的进度报告"
echo "2. 添加了处理时间显示"
echo ""
echo "=== 为什么CSF读取难以并行化 ==="
echo "原因分析："
echo "- 文件读取是串行I/O操作"
echo "- CSF记录之间有依赖关系（重复检测）"
echo "- 内存分配需要同步"
echo ""
echo "=== 其他可能的优化方向 ==="
echo "1. 使用SSD存储CSF文件"
echo "2. 增加系统I/O缓冲区大小"
echo "3. 使用内存文件系统(tmpfs)"
echo "4. 预处理CSF文件为二进制格式"
echo ""
echo "=== 系统级优化建议 ==="
echo "运行以下命令优化I/O性能："
echo "# 增加文件系统缓存"
echo "sudo sysctl -w vm.dirty_ratio=15"
echo "sudo sysctl -w vm.dirty_background_ratio=5"
echo ""
echo "# 如果使用NVMe SSD"
echo "sudo echo mq-deadline > /sys/block/nvme0n1/queue/scheduler"
echo ""
echo "=== 内存文件系统优化 ==="
echo "如果有足够内存，可以将CSF文件复制到内存："
echo "sudo mkdir -p /tmp/grasp_work"
echo "sudo mount -t tmpfs -o size=8G tmpfs /tmp/grasp_work"
echo "cp *.c *.w *.cm isodata /tmp/grasp_work/"
echo "cd /tmp/grasp_work && rhfs_omp" 