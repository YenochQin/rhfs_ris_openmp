# GRASP程序包 OpenMP 并行化版本使用说明

## 概述

此版本的GRASP程序包已经通过OpenMP进行并行化，包含以下三个主要计算程序：

- **RHFS90**：相对论超精细结构计算程序
- **RHFSZEEMAN95**：超精细结构和塞曼效应计算程序  
- **RIS4**：同位素位移计算程序

所有程序都可以在多核处理器上实现更快的计算速度。

## 支持的程序

### 1. RHFS90 (rhfs)
- **功能**：计算原子的超精细结构常数
- **输出文件**：`.h`（超精细常数）、`.hoffd`（off-diagonal元素）、`.sum`（计算摘要）
- **适用场景**：基本的超精细结构计算

### 2. RHFSZEEMAN95 (rhfszeeman95)  
- **功能**：计算超精细结构和塞曼效应
- **输出文件**：`.h`（结果）、`.gjhfs`（g因子和超精细常数）、`.sum`（计算摘要）
- **适用场景**：需要考虑外磁场效应的计算

### 3. RIS4 (ris4)
- **功能**：计算同位素位移的电子因子
- **输出文件**：`.i`（同位素位移参数）、`.sum`（计算摘要）、`.IOB`/.ITB（角度系数）
- **适用场景**：同位素位移和场位移的理论计算

## 并行化改进

### 1. 主要并行化区域

**径向积分和角度矩阵元素计算**：
- 使用`!$OMP PARALLEL DO`并行化了`RINTME`、`RINTGJ`、`RINTDGJ`、`AMELT`、`GJMELT`和`DGJMELT`数组的计算
- 采用`COLLAPSE(2)`指令来并行化嵌套循环
- 使用`SCHEDULE(DYNAMIC)`来实现动态负载均衡

**主计算循环**：
- 对哈密顿矩阵构建的主要双重循环（IC和IR）进行并行化
- 使用临界区（`!$OMP CRITICAL`）保护共享变量的写入操作
- 采用动态调度以平衡不同线程间的工作负载

### 2. 性能优化特性

- **动态调度**：自动平衡线程间的工作负载
- **变量作用域控制**：明确指定共享和私有变量
- **临界区保护**：确保并行安全的内存访问
- **线程安全的输出**：只允许主线程输出进度信息

## 编译选项

### 使用CMake构建（推荐）

**自动检测OpenMP并行版本编译**：
```bash
# 使用专用的OpenMP构建脚本
./build_openmp.sh

# 或者手动使用CMake
mkdir build && cd build
cmake ..
make -j$(nproc)
make install
cd ..
```

**编译特定程序**：
```bash
# 编译所有程序
./build_openmp.sh

# 清理后重新编译
./build_openmp.sh --clean

# Debug版本编译
./build_openmp.sh --debug

# 禁用OpenMP（串行版本）
./build_openmp.sh --no-openmp
```

### 传统Makefile构建（兼容性）

```bash
# 并行版本
make parallel

# 串行版本  
make serial

# 查看编译配置
make config
```

## 运行时配置

### 1. 基本运行方法

**直接运行可执行文件**：
```bash
# 设置线程数
export OMP_NUM_THREADS=4

# 运行不同程序
./bin/rhfs           # RHFS90超精细结构计算
./bin/rhfszeeman95   # 超精细和塞曼效应计算  
./bin/ris4           # 同位素位移计算
```

**使用并行运行脚本（推荐）**：
```bash
# 语法: ./run_parallel.sh [程序名] [线程数] [输入文件名]

# 运行RHFS90，使用4线程
./run_parallel.sh rhfs 4 my_atom

# 运行RHFSZEEMAN95，使用8线程
./run_parallel.sh rhfszeeman95 8 my_atom

# 运行RIS4，使用2线程
./run_parallel.sh ris4 2 my_atom

# 向后兼容：默认运行RHFS90
./run_parallel.sh 4 my_atom
```

### 2. 设置线程数
```bash
# 设置使用4个线程
export OMP_NUM_THREADS=4

# 或者在运行时指定
OMP_NUM_THREADS=8 ./bin/rhfs
```

### 2. 推荐配置

**小规模计算**（NCF < 1000）：
```bash
export OMP_NUM_THREADS=2-4
```

**中等规模计算**（1000 ≤ NCF < 10000）：
```bash
export OMP_NUM_THREADS=4-8
```

**大规模计算**（NCF ≥ 10000）：
```bash
export OMP_NUM_THREADS=8-16
```

### 3. 内存绑定策略
```bash
export OMP_PROC_BIND=true
export OMP_PLACES=cores
```

## 性能监控

### 1. 基本性能信息
各程序运行时会显示使用的线程数：
```
RHFS: Using OpenMP with 8 threads
RHFSZEEMAN95: Using OpenMP with 8 threads  
RIS4 DENSNEW: Using OpenMP with 8 threads
RIS4 SMSNEW: Using OpenMP with 8 threads
```

### 2. 详细性能分析
使用系统工具监控：
```bash
# 监控CPU使用率
htop

# 监控内存使用
free -h

# 使用time命令测量运行时间
time ./bin/rhfs
time ./bin/rhfszeeman95
time ./bin/ris4
```

## 预期性能提升

根据问题规模和硬件配置，典型的性能提升如下：

| 线程数 | 预期加速比 | 适用场景 |
|--------|------------|----------|
| 2      | 1.5-1.8x   | 双核处理器 |
| 4      | 2.5-3.2x   | 四核处理器 |
| 8      | 4.0-5.5x   | 八核处理器 |
| 16     | 6.0-9.0x   | 高端工作站 |

**注意**：实际性能提升取决于：
- 问题规模（NCF的大小）
- 硬件配置（CPU核心数、内存带宽）
- 系统负载情况

## 故障排除

### 1. 编译错误
如果遇到OpenMP编译错误：
```bash
# 检查编译器是否支持OpenMP
gfortran --version
ifort --version

# 使用串行版本作为备选
make clean
make serial
```

### 2. 运行时问题

**内存不足**：
```bash
# 减少线程数
export OMP_NUM_THREADS=2
```

**性能下降**：
```bash
# 检查是否超线程干扰
export OMP_NUM_THREADS=物理核心数
```

### 3. 结果验证
确保并行版本和串行版本产生相同结果：
```bash
# 运行串行版本
./build_openmp.sh --no-openmp
./bin/rhfs > serial_output.log

# 运行并行版本  
./build_openmp.sh
OMP_NUM_THREADS=4 ./bin/rhfs > parallel_output.log

# 比较结果
diff serial_output.log parallel_output.log
```

**验证其他程序**：
```bash
# RHFSZEEMAN95验证
OMP_NUM_THREADS=1 ./bin/rhfszeeman95 > rhfszeeman95_serial.log
OMP_NUM_THREADS=4 ./bin/rhfszeeman95 > rhfszeeman95_parallel.log
diff rhfszeeman95_serial.log rhfszeeman95_parallel.log

# RIS4验证  
OMP_NUM_THREADS=1 ./bin/ris4 > ris4_serial.log
OMP_NUM_THREADS=4 ./bin/ris4 > ris4_parallel.log
diff ris4_serial.log ris4_parallel.log
```

## 技术细节

### 并行化策略
1. **任务并行**：预计算阶段的独立计算任务
2. **数据并行**：主循环中的矩阵元素计算
3. **同步控制**：使用临界区保护共享数据

### 内存访问模式
- 读取操作：并行安全
- 写入操作：通过临界区保护
- 数组访问：优化了缓存局部性

### 负载均衡
- 动态调度：自适应工作负载分配
- 任务粒度：针对不同循环优化

## 兼容性

- **编译器**：支持gfortran 4.9+, Intel Fortran, 等
- **操作系统**：Linux, macOS, Windows (MinGW)
- **OpenMP版本**：2.0或更高版本

## 许可证

本并行化版本遵循原始GRASP92程序的许可证条款。

---

*最后更新：2024年12月* 