library(RUnit)

test.suite <- defineTestSuite("nctx-tests",
                              dirs = file.path("testnctx"),
#~                               testFileRegexp = '^4\\.R$')
                              testFileRegexp = '^\\d+\\.R$')

test.result <- runTestSuite(test.suite)
 
printTextProtocol(test.result)
