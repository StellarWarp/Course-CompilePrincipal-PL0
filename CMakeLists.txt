# 设置 CMake 最低版本要求
cmake_minimum_required(VERSION 3.10)

# 设置项目名称和版本
set(PROJECT_NAME CP_EXP)
project(${PROJECT_NAME} VERSION 1.0)

# 设置 C++ 标准
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# 添加头文件路径
include_directories(${PROJECT_SOURCE_DIR})

# 添加源文件
file(GLOB SOURCES "*.cpp" "*.h")

# 生成可执行文件
add_executable(${PROJECT_NAME} ${SOURCES})