# RHFS OpenMP 线程安全问题

## 问题描述

rhfs程序在多线程环境下会卡住，原因是存在线程安全问题。主要涉及的函数：

1. `ONEPARTICLEJJ` - 修改全局数组 JLIST, JJC1, JJC2, JLIS, JC1S, JC2S
2. `SETQNA` - 修改全局数组 NQ1, NQ2, JJQ1, JJQ2, JLIST, KLIST 和全局变量 NPEEL, NCORE

## 全局状态冲突

以下模块包含被多线程同时修改的全局状态：
- `m_C` 模块：NQ1, NQ2, JJC1, JJC2, JJQ1, JJQ2, JLIST, KLIST, NPEEL, NCORE
- `dumx_C` 模块：JLIS, JC1S, JC2S

## 当前解决方案

### 临时解决方案
强制使用单线程执行：
```fortran
CALL OMP_SET_NUM_THREADS(1)
```

### 推荐的长期解决方案

1. **重构全局状态为线程局部存储**
   - 将全局数组移到函数参数中
   - 使用 `!$OMP THREADPRIVATE` 指令
   - 创建每个线程的私有副本

2. **函数重新设计**
   - 修改 ONEPARTICLEJJ 和 SETQNA 函数为纯函数（无副作用）
   - 通过参数传递所有必要的数据
   - 避免全局状态依赖

3. **更精细的锁定策略**
   - 为每个全局数组使用独立的锁
   - 减少临界区的大小
   - 使用原子操作替代锁

## 实施建议

1. **阶段1**：确保单线程正确性（当前）
2. **阶段2**：重构数据结构，消除全局依赖
3. **阶段3**：重新实现OpenMP并行化

## 性能影响

- 当前单线程解决方案：保证正确性，但无并行加速
- 长期解决方案：可实现真正的多线程并行加速

## 测试验证

运行测试用例验证：
```bash
export OMP_NUM_THREADS=1  # 强制单线程
./rhfs90 < input_file
```

多线程测试（修复后）：
```bash
export OMP_NUM_THREADS=4
./rhfs90 < input_file
``` 