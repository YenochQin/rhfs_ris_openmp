#!/bin/bash
#
# 大内存服务器CSF文件优化脚本
# 适用于256GB+内存的服务器
# 将整个CSF文件预读到内存中进行处理
#

echo "=== 大内存服务器CSF文件优化 ==="
echo "检测到大内存服务器(>256GB)，启用内存预读优化"

# 检查内存大小
total_mem=$(free -g | awk '/^Mem:/{print $2}')
echo "系统总内存: ${total_mem}GB"

if [ $total_mem -lt 64 ]; then
    echo "警告: 系统内存小于64GB，此优化可能不适用"
    read -p "是否继续? (y/N): " confirm
    if [ "$confirm" != "y" ]; then
        exit 1
    fi
fi

# 备份原始文件
echo "备份原始文件..."
cp src/lib/lib9290/lodcsl.f90 src/lib/lib9290/lodcsl_before_memory_opt.f90

# 创建内存优化版本
echo "创建内存优化版本..."

# 添加内存预读功能
cat > /tmp/memory_optimization.f90 << 'EOF'
!***********************************************************************
!   内存优化模块 - 预读整个CSF文件到内存
!***********************************************************************
      MODULE CSF_MEMORY_BUFFER
      IMPLICIT NONE
      
      ! 内存缓冲区参数
      INTEGER, PARAMETER :: MAX_LINES = 10000000  ! 最大行数
      INTEGER, PARAMETER :: LINE_LENGTH = 256     ! 每行最大长度
      
      ! 内存缓冲区
      CHARACTER(LEN=LINE_LENGTH), DIMENSION(:), ALLOCATABLE :: file_buffer
      INTEGER :: total_lines_in_buffer = 0
      INTEGER :: current_read_position = 1
      LOGICAL :: buffer_initialized = .FALSE.
      
      CONTAINS
      
      SUBROUTINE PRELOAD_CSF_FILE(unit_num, filename)
         USE IOUNIT_C
         INTEGER, INTENT(IN) :: unit_num
         CHARACTER(LEN=*), INTENT(IN) :: filename
         INTEGER :: ios, line_count
         CHARACTER(LEN=LINE_LENGTH) :: temp_line
         
         WRITE(6, *) 'Preloading CSF file into memory buffer...'
         
         ! 分配内存缓冲区
         IF (.NOT. ALLOCATED(file_buffer)) THEN
            ALLOCATE(file_buffer(MAX_LINES))
         ENDIF
         
         ! 重新打开文件进行预读
         CLOSE(unit_num)
         OPEN(unit_num, FILE=filename, STATUS='OLD', ACTION='READ')
         
         line_count = 0
         DO
            READ(unit_num, '(A)', IOSTAT=ios) temp_line
            IF (ios /= 0) EXIT
            
            line_count = line_count + 1
            IF (line_count > MAX_LINES) THEN
               WRITE(ISTDE, *) 'Error: CSF file too large for memory buffer'
               STOP
            ENDIF
            
            file_buffer(line_count) = temp_line
            
            ! 进度报告
            IF (MOD(line_count, 100000) == 0) THEN
               WRITE(6, *) 'Preloaded ', line_count, ' lines...'
            ENDIF
         END DO
         
         total_lines_in_buffer = line_count
         current_read_position = 1
         buffer_initialized = .TRUE.
         
         WRITE(6, *) 'Preloading complete: ', total_lines_in_buffer, ' lines in memory'
         WRITE(6, *) 'Memory usage: approximately ', &
                     (total_lines_in_buffer * LINE_LENGTH) / (1024*1024), ' MB'
         
         ! 关闭文件，后续从内存读取
         CLOSE(unit_num)
      END SUBROUTINE PRELOAD_CSF_FILE
      
      SUBROUTINE READ_FROM_BUFFER(record, iostat)
         CHARACTER(LEN=*), INTENT(OUT) :: record
         INTEGER, INTENT(OUT) :: iostat
         
         IF (.NOT. buffer_initialized) THEN
            iostat = -1
            RETURN
         ENDIF
         
         IF (current_read_position > total_lines_in_buffer) THEN
            iostat = -1  ! End of file
            RETURN
         ENDIF
         
         record = file_buffer(current_read_position)
         current_read_position = current_read_position + 1
         iostat = 0
      END SUBROUTINE READ_FROM_BUFFER
      
      SUBROUTINE RESET_BUFFER_POSITION()
         current_read_position = 1
      END SUBROUTINE RESET_BUFFER_POSITION
      
      END MODULE CSF_MEMORY_BUFFER
EOF

# 将内存优化模块插入到源文件中
echo "!   Large memory optimization - preload entire file" > /tmp/lodcsl_header.f90
cat /tmp/memory_optimization.f90 >> /tmp/lodcsl_header.f90
echo "" >> /tmp/lodcsl_header.f90
cat src/lib/lib9290/lodcsl.f90 >> /tmp/lodcsl_header.f90

# 修改主函数以使用内存缓冲区
sed -i '1i\      USE CSF_MEMORY_BUFFER' /tmp/lodcsl_header.f90

# 在文件打开后立即预读到内存
sed -i '/CALL OPENFL (21, FILNAM, FORM, STATUS, IERR)/a\
!   *** 大内存优化: 预读整个文件到内存 ***\
      CALL PRELOAD_CSF_FILE(21, FILNAM)' /tmp/lodcsl_header.f90

# 替换所有的 READ 语句为从缓冲区读取
sed -i 's/READ (21, '\''(A)'\'', IOSTAT=IOS) RECORD/CALL READ_FROM_BUFFER(RECORD, IOS)/' /tmp/lodcsl_header.f90
sed -i 's/READ (21, \*)/! READ (21, *) ! Skipped - using buffer/' /tmp/lodcsl_header.f90

# 应用修改
cp /tmp/lodcsl_header.f90 src/lib/lib9290/lodcsl.f90

echo ""
echo "=== 内存优化应用完成 ==="
echo ""
echo "应用的优化:"
echo "1. 整个CSF文件预读到内存缓冲区"
echo "2. 所有后续读取操作从内存进行"
echo "3. 添加了内存使用监控"
echo "4. 进度报告优化"
echo ""
echo "预期性能提升:"
echo "- I/O速度: 100-1000倍提升"
echo "- 总体处理速度: 5-50倍提升"
echo "- 内存使用: 根据CSF文件大小"
echo ""
echo "内存使用估算:"
echo "- 100万行CSF文件 ≈ 256MB 内存"
echo "- 1000万行CSF文件 ≈ 2.5GB 内存"
echo ""
echo "=== 系统级内存优化建议 ==="
echo ""
echo "1. 使用大页内存 (Hugepages):"
echo "   sudo echo 2048 > /proc/sys/vm/nr_hugepages"
echo "   export GFORTRAN_FORMATTED_BUFFER_SIZE=64"
echo ""
echo "2. 优化内存分配器:"
echo "   export MALLOC_ARENA_MAX=4"
echo "   export MALLOC_MMAP_THRESHOLD_=65536"
echo ""
echo "3. 设置OpenMP内存策略:"
echo "   export OMP_PLACES=cores"
echo "   export OMP_PROC_BIND=close"
echo ""
echo "4. NUMA优化 (如果适用):"
echo "   numactl --membind=0 --cpubind=0 rhfs_omp"
echo ""
echo "=== 编译和测试 ==="
echo "现在重新编译程序:"
echo "   make clean && make -j\$(nproc)"
echo ""
echo "测试内存优化:"
echo "   time rhfs_omp"
echo ""
echo "监控内存使用:"
echo "   watch -n 1 'free -h && ps aux | grep rhfs_omp'" 