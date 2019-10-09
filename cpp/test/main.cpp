#include <gtest/gtest.h>

#define APPROVALS_GOOGLETEST_EXISTING_MAIN
#include "../third_party/ApprovalTests.v.5.1.0.hpp"

int main(int argc, char** argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    ApprovalTests::initializeApprovalTestsForGoogleTests();
    return RUN_ALL_TESTS();
}

