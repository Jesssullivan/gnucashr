cmake_minimum_required(VERSION 3.16)
project(gnucash-core VERSION 0.1.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# --- Find sqlite3 ---
find_package(PkgConfig REQUIRED)
pkg_check_modules(SQLITE3 REQUIRED sqlite3)

# --- Find nlohmann_json (for bridge + tests) ---
find_package(nlohmann_json 3 QUIET)
if(NOT nlohmann_json_FOUND)
    include(FetchContent)
    FetchContent_Declare(
        nlohmann_json
        GIT_REPOSITORY https://github.com/nlohmann/json.git
        GIT_TAG v3.11.3
        GIT_SHALLOW TRUE
    )
    FetchContent_MakeAvailable(nlohmann_json)
endif()

# --- Library ---
add_library(gnucash-core
    src/fraction.cpp
    src/guid.cpp
    src/types.cpp
    src/book.cpp
    src/ofx.cpp
    src/audit.cpp
)

target_include_directories(gnucash-core
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
    PRIVATE
        ${SQLITE3_INCLUDE_DIRS}
)

target_link_libraries(gnucash-core
    PRIVATE ${SQLITE3_LIBRARIES}
)

target_link_directories(gnucash-core
    PRIVATE ${SQLITE3_LIBRARY_DIRS}
)

target_compile_features(gnucash-core PUBLIC cxx_std_17)

# --- Tests ---
option(GNUCASH_CORE_BUILD_TESTS "Build tests" ON)

if(GNUCASH_CORE_BUILD_TESTS)
    enable_testing()

    # Try system Catch2 first, fall back to FetchContent
    find_package(Catch2 3 QUIET)
    if(NOT Catch2_FOUND)
        include(FetchContent)
        FetchContent_Declare(
            Catch2
            GIT_REPOSITORY https://github.com/catchorg/Catch2.git
            GIT_TAG v3.5.2
            GIT_SHALLOW TRUE
        )
        FetchContent_MakeAvailable(Catch2)
        list(APPEND CMAKE_MODULE_PATH ${catch2_SOURCE_DIR}/extras)
    endif()

    add_executable(gnucash-core-tests
        test/test_result.cpp
        test/test_fraction.cpp
        test/test_guid.cpp
        test/test_types.cpp
        test/test_book.cpp
        test/test_json_api.cpp
        test/test_ofx.cpp
        src/json_api.cpp
    )

    # Pass fixture path as compile definition
    target_compile_definitions(gnucash-core-tests PRIVATE
        FIXTURE_DIR="${CMAKE_CURRENT_SOURCE_DIR}/test/fixtures"
    )

    target_link_libraries(gnucash-core-tests
        PRIVATE
            gnucash-core
            Catch2::Catch2WithMain
            nlohmann_json::nlohmann_json
            ${SQLITE3_LIBRARIES}
    )

    target_link_directories(gnucash-core-tests
        PRIVATE ${SQLITE3_LIBRARY_DIRS}
    )

    include(Catch)
    catch_discover_tests(gnucash-core-tests)
endif()

# --- JSON API Bridge executable ---
option(GNUCASH_CORE_BUILD_BRIDGE "Build JSON bridge executable" ON)

if(GNUCASH_CORE_BUILD_BRIDGE)
    add_executable(gnucash-bridge
        src/json_api.cpp
        src/mcp.cpp
        src/main.cpp
    )

    target_include_directories(gnucash-bridge PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/src
    )

    target_link_libraries(gnucash-bridge
        PRIVATE
            gnucash-core
            nlohmann_json::nlohmann_json
            ${SQLITE3_LIBRARIES}
    )

    target_link_directories(gnucash-bridge
        PRIVATE ${SQLITE3_LIBRARY_DIRS}
    )

    install(TARGETS gnucash-bridge RUNTIME DESTINATION bin)
endif()

# --- Install ---
install(TARGETS gnucash-core
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
)

install(DIRECTORY include/gnucash
    DESTINATION include
)
