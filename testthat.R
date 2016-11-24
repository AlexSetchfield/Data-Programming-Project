library(testthat)
library(workflow)

# DNAaa() test
context("DNAaatests")
test_that("DNAaatest1",{
  amino1<-DNAaa("123atggta")
  expect_that(amino1$aa_number,equals(2))
  expect_that(amino1$length_of_gene,equals(6))
  expect_that(amino1$aa_seq,equals("MV"))
})

test_that("DNAaatest1",{
  amino2<-DNAaa("ATGGTA")
  expect_that(amino2$aa_number,equals(2))
  expect_that(amino2$length_of_gene,equals(6))
  expect_that(amino2$aa_seq,equals("MV"))
  })

# mw() test
context("mwtests")
test_that("mwtest", {
  expect_that(mw("VH"), prints_text("aa sequence length =  2", "Molecular Weight prediction = 0.29 kDa"))
  expect_that(mw("vh"), prints_text("aa sequence length =  2", "Molecular Weight prediction = 0.29 kDa"))
  expect_that(mw("123vh"), prints_text("aa sequence length =  2", "Molecular Weight prediction = 0.29 kDa"))
  })

# gram.calc() test
context("gram.calctests")
test_that("gram.calctest", {
  expect_equivalent(gram.calc("DTT", 5, 25, 58.44), "DTT = 0.007305 g")
})

# OD600() test
context("OD600tests")
test_that("OD600test", {
  expect_that(OD600(1.0, accurate = T, 100, 0.3, 200, 0.5), prints_text("Need 6 Hour 40 min to reach OD 1"))
  expect_that(OD600(1.0, accurate = F, 100), prints_text("Need 12 Hour 55 min to reach OD 1"))
  expect_that(OD600(1.0, 100), prints_text("Need 12 Hour 55 min to reach OD 1"))
  expect_that(OD600(1.0, 100000), prints_text("According to mathematic model, the OD600 of your culture is 2.5","According to mathematic model, the OD600 of your culture is 2.5"))
  expect_that(OD600(1.0,accurate = F, 100000), prints_text("According to mathematic model, the OD600 of your culture is 2.5","According to mathematic model, the OD600 of your culture is 2.5"))
  expect_that(OD600(1.0,accurate=T,100,3,120,3.5), prints_text("The calculated time is negative, please enter the correct data"))
  })

# lbmasscalc() test
context("lbmasscalctests")
test_that("lbmasscalctest", {
  masses <- lbmasscalc(TrisHCl.conc = 50, NaCl.conc = 200, DTT.conc = 5, PMSF.conc = 1, volume = 1000)
  expect_that(masses$"Tris Mass (g)", equals(6.057))
  expect_that(masses$"NaCl Mass (g)", equals(11.688))
  expect_that(masses$"DTT Mass (g)", equals(0.771265))
  expect_that(masses$"PMSF Mass (g)", equals(0.17419))
})
