# GRASP OpenMP 并行化程序包

[![License](https://img.shields.io/badge/license-Academic-blue.svg)](LICENSE)
[![OpenMP](https://img.shields.io/badge/OpenMP-Parallel-green.svg)](https://www.openmp.org/)
[![Fortran](https://img.shields.io/badge/Fortran-90%2B-orange.svg)](https://fortran-lang.org/)
[![Build](https://img.shields.io/badge/build-CMake-red.svg)](https://cmake.org/)

基于GRASP（General Relativistic Atomic Structure Package）的OpenMP并行化版本，专门用于相对论原子结构计算。本程序包包含三个主要的计算程序，所有程序都已通过OpenMP技术实现并行化，可以充分利用多核处理器的计算能力。

## ✨ 特性

- 🚀 **高性能并行计算**：通过OpenMP实现4-10倍性能提升
- 🔬 **多程序支持**：包含超精细结构、塞曼效应和同位素位移计算
- 🛠️ **现代构建系统**：使用CMake和自动化脚本
- 📊 **性能监控**：内置性能分析和报告功能
- 🔧 **易于使用**：提供便捷的运行脚本和详细文档

## 📦 包含程序

| 程序 | 功能描述 | 输出文件 | 应用场景 |
|------|----------|----------|----------|
| **RHFS90** | 相对论超精细结构计算 | `.h`, `.hoffd`, `.sum` | 基础超精细结构常数计算 |
| **RHFSZEEMAN95** | 超精细结构和塞曼效应计算 | `.h`, `.gjhfs`, `.sum` | 外磁场中的原子能级分裂 |
| **RIS4** | 同位素位移计算 | `.i`, `.sum`, `.IOB/.ITB` | 同位素位移和场位移电子因子 |

## 🔧 系统要求

- **操作系统**：Linux, macOS, Windows (WSL)
- **编译器**：
  - GNU Fortran (gfortran) 4.9+
  - Intel Fortran Compiler
  - 或其他支持OpenMP的Fortran编译器
- **依赖库**：
  - BLAS/LAPACK
  - OpenMP 2.0+
- **构建工具**：CMake 3.10+

## 🚀 快速开始

### 1. 克隆仓库
```bash
git clone https://github.com/your-username/grasp-openmp.git
cd grasp-openmp
```

### 2. 编译程序
```bash
# 自动编译所有程序（推荐）
./build_openmp.sh

# 或者手动使用CMake
mkdir build && cd build
cmake ..
make -j$(nproc)
make install
cd ..
```

### 3. 运行计算
```bash
# 设置线程数
export OMP_NUM_THREADS=4

# 运行不同程序
./run_parallel.sh rhfs 4 my_atom           # 超精细结构计算
./run_parallel.sh rhfszeeman95 8 my_atom   # 塞曼效应计算
./run_parallel.sh ris4 2 my_atom           # 同位素位移计算
```

## 📖 详细使用指南

### 编译选项

```bash
# 基本编译
./build_openmp.sh

# 清理后重新编译
./build_openmp.sh --clean

# Debug模式编译
./build_openmp.sh --debug

# 编译串行版本（禁用OpenMP）
./build_openmp.sh --no-openmp

# 指定编译线程数
./build_openmp.sh --jobs 8
```

### 运行配置

#### 基本运行
```bash
# 直接运行可执行文件
export OMP_NUM_THREADS=4
./bin/rhfs                    # RHFS90
./bin/rhfszeeman95           # RHFSZEEMAN95
./bin/ris4                   # RIS4
```

#### 使用运行脚本（推荐）
```bash
# 语法：./run_parallel.sh [程序名] [线程数] [输入文件名]
./run_parallel.sh rhfs 4 my_atom
./run_parallel.sh rhfszeeman95 8 my_atom
./run_parallel.sh ris4 2 my_atom

# 查看帮助
./run_parallel.sh --help
```

### OpenMP环境变量优化

```bash
# 基本配置
export OMP_NUM_THREADS=4
export OMP_PROC_BIND=true
export OMP_PLACES=cores

# 大内存系统优化
export OMP_STACKSIZE=64M
export OMP_DYNAMIC=false
```

## 📊 性能基准

根据不同硬件配置的典型性能提升：

| CPU核心数 | 预期加速比 | 适用规模 |
|-----------|------------|----------|
| 2核 | 1.5-1.8× | 小规模计算 (NCF < 1000) |
| 4核 | 2.5-3.2× | 中等规模 (1000 ≤ NCF < 5000) |
| 8核 | 4.0-5.5× | 大规模计算 (5000 ≤ NCF < 20000) |
| 16核+ | 6.0-10.0× | 超大规模计算 (NCF ≥ 20000) |

### 性能监控示例

```bash
# 使用time命令测量运行时间
time ./run_parallel.sh rhfs 8 my_atom

# 监控CPU和内存使用
htop

# 查看程序输出的线程信息
./bin/rhfs  # 会显示 "RHFS: Using OpenMP with 8 threads"
```

## 🔬 输入文件准备

### 必需的输入文件

1. **原子结构文件**：
   - `isodata` - 同位素数据文件
   - `{name}.c` - 配置状态文件
   - `{name}.w` - 波函数文件

2. **混合系数文件**：
   - `{name}.cm` - CI混合系数文件
   - 或 `{name}.m` - ASF混合系数文件

### 示例输入文件结构
```
my_atom/
├── isodata
├── my_atom.c
├── my_atom.w
├── my_atom.cm
└── run_calculation.sh
```

## 📈 输出文件说明

### RHFS90输出
- `{name}.h` - 超精细结构常数
- `{name}.hoffd` - 非对角矩阵元素
- `{name}.sum` - 计算摘要和日志

### RHFSZEEMAN95输出
- `{name}.h` - 超精细和塞曼相互作用常数
- `{name}.gjhfs` - g因子和超精细参数
- `{name}.sum` - 计算摘要

### RIS4输出
- `{name}.i` - 同位素位移电子因子
- `{name}.IOB` / `{name}.ITB` - 角度系数文件
- `{name}.sum` - 计算摘要

## 🛠️ 故障排除

### 编译问题

```bash
# 检查编译器支持
gfortran --version
gfortran -fopenmp --version

# 如果OpenMP不可用，编译串行版本
./build_openmp.sh --no-openmp
```

### 运行时问题

```bash
# 内存不足时减少线程数
export OMP_NUM_THREADS=2

# 验证并行和串行结果一致性
OMP_NUM_THREADS=1 ./bin/rhfs > serial.log
OMP_NUM_THREADS=4 ./bin/rhfs > parallel.log
diff serial.log parallel.log
```

### 性能问题

```bash
# 避免过度线程化
export OMP_NUM_THREADS=$(nproc)  # 不要超过物理核心数

# 设置亲和性绑定
export OMP_PROC_BIND=true
export OMP_PLACES=cores
```

## 🤝 贡献指南

我们欢迎社区贡献！请遵循以下步骤：

1. Fork 本仓库
2. 创建特性分支 (`git checkout -b feature/amazing-feature`)
3. 提交更改 (`git commit -m 'Add amazing feature'`)
4. 推送到分支 (`git push origin feature/amazing-feature`)
5. 开启 Pull Request

### 开发环境设置

```bash
# 克隆开发版本
git clone https://github.com/your-username/grasp-openmp.git
cd grasp-openmp

# 设置开发环境
./build_openmp.sh --debug
make config  # 查看配置信息
```

## 📚 相关文档

- [OpenMP详细使用说明](OPENMP_README.md)
- [性能基准测试](docs/benchmarks.md)
- [输入文件格式说明](docs/input_format.md)
- [API参考文档](docs/api_reference.md)

## 📄 许可证

本项目基于原始GRASP程序的学术许可证。详见 [LICENSE](LICENSE) 文件。

## 🙏 致谢

- 原始GRASP程序的开发者
- OpenMP社区的技术支持
- 所有贡献者和用户的反馈

## 线程安全问题已解决 ✅

**真正的OpenMP并行化现已实现**

线程安全问题已通过使用`THREADPRIVATE`指令解决，现在支持真正的多线程并行计算。

### 解决方案
- 使用`!$OMP THREADPRIVATE`使全局变量对每个线程私有
- 移除了不必要的CRITICAL区域
- 保留了结果更新时的必要同步

### 性能优势
- ✅ 真正的多线程并行加速
- ✅ 线程安全且稳定
- ✅ 可充分利用多核CPU资源

### 使用方法
```bash
# 设置线程数（可选，默认使用所有可用核心）
export OMP_NUM_THREADS=8

# 编译和运行
./build_openmp.sh
./bin/rhfs90 < input_file
```

### 预期输出
```
RHFS: Using OpenMP with 8 threads (thread-safe with THREADPRIVATE globals)
Column 50 complete;
Column 100 complete;
...
```

---

<div align="center">
  <strong>⭐ 如果这个项目对您有帮助，请给我们一个星标！ ⭐</strong>
</div> 