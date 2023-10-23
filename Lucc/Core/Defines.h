#pragma once
#include <cassert>

#define _STRINGIFY_IMPL(...) #__VA_ARGS__
#define _CONCAT_IMPL(x, y) x##y
#define LU_STRINGIFY(...) _STRINGIFY_IMPL(__VA_ARGS__)
#define LU_CONCAT(x, y) _CONCAT_IMPL( x, y )

#define LU_ASSERT(expr) assert(expr)
#define LU_ASSERT_MSG(expr, msg) assert(expr && msg)
#define LU_OPTIMIZE_ON  #pragma optimize("", on)
#define LU_OPTIMIZE_OFF #pragma optimize("", off)
#define LU_WARNINGS_OFF #pragma(warning(push, 0))
#define LU_WARNINGS_ON  #pragma(warning(pop))
#define LU_DEBUGBREAK() __debugbreak()
#define LU_FORCEINLINE	__forceinline
#define LU_UNREACHABLE() __assume(false)
#define LU_INLINE				inline
#define LU_NODISCARD			[[nodiscard]]
#define LU_NORETURN				[[noreturn]]
#define LU_DEPRECATED			[[deprecated]]
#define LU_DEPRECATED_MSG(msg)	[[deprecated(#msg)]]
#define LU_ALIGNAS(align)       alignas(align) 

#define LU_ALIGN(n, align) ((n + align - 1) / align * align)

#define LU_NONCOPYABLE(ClassName)                 \
    ClassName(ClassName const&)            = delete; \
    ClassName& operator=(ClassName const&) = delete;

#define LU_NONMOVABLE(ClassName)                      \
    ClassName(ClassName&&) noexcept            = delete; \
    ClassName& operator=(ClassName&&) noexcept = delete;

#define LU_NONCOPYABLE_NONMOVABLE(ClassName) \
        LU_NONCOPYABLE(ClassName)                \
        LU_NONMOVABLE(ClassName)

#define LU_DEFAULT_COPYABLE(ClassName)             \
    ClassName(ClassName const&)            = default; \
    ClassName& operator=(ClassName const&) = default;

#define LU_DEFAULT_MOVABLE(ClassName)                  \
    ClassName(ClassName&&) noexcept            = default; \
    ClassName& operator=(ClassName&&) noexcept = default;

#define LU_DEFAULT_COPYABLE_MOVABLE(ClassName) \
    LU_DEFAULT_COPYABLE(ClassName)             \
    LU_DEFAULT_MOVABLE(ClassName)

