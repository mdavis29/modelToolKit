##buildFile

irs$The.State.Federal.Information.Processing.System..FIPS..code<-NULL
irs$Size.of.adjusted.gross.income<-NULL

keep<-c(
  "Income.tax.amount..8..." ,
  "Net.investment.income.tax.amount",
  "Total.tax.liability.amount..9...",
  "Number.of.returns.with.income.tax",
  "Number.of.returns.with.net.premium.tax.credit" ,
  "Number.of.returns.with.additional.child.tax.credit",
  "Number.of.returns.with.earned.income.credit",
  "Total.tax.payments.amount",
  "Number.of.returns.with.self.employment.tax",
  "Number.of.returns.with.child.tax.credit",
  "Number.of.returns.with.retirement.savings.contribution.credit",
  "Number.of.returns.with.foreign.tax.credit"  ,
  "Foreign.tax.credit.amount",
  "Real.estate.taxes.amount",
  "Salaries.and.wages.amount",
  "Number.of.returns.with.real.estate.taxes",
  "State.and.local.income.taxes.amount",
  "Total.itemized.deductions.amount" ,
  "Tuition.and.fees.deduction.amount",
  "Student.loan.interest.deduction.amount" ,
  "Number.of.returns.with.IRA.payments" ,
  "Number.of.returns.with.self.employment.health.insurance.deduction",
  "Number.of.returns.with.taxable.Social.Security.benefits",
  "Number.of.returns.with.unemployment.compensation",
  "Unemployment.compensation.amount..5.",
  "Number.of.farm.returns",
  "Number.of.returns.with.mortgage.interest.paid" ,
  "Number.of.returns.with.taxable.pensions.and.annuities",
  "Number.of.returns.with.qualified.dividends" ,
  "Number.of.returns.with.ordinary.dividends.",
  "Total.income.amount"   ,
  "Number.of.returns.with.taxable.interest",
  "Number.of.dependents" ,
  "Adjust.gross.income..AGI...2."    ,
  "zipCode" ,
  "stateCode" ,
  "Number.of.returns")

zipCode.aggSum<-aggregate(.~zipCode + stateCode,
                          data = irs[,keep],
                          FUN = sum,
                          na.rm =TRUE)

demCols<-keep[!keep %in% c("zipCode", "stateCode")]
n<-zipCode.aggSum$Number.of.returns

zipCode.aggSum[,demCols] <- zipCode.aggSum[,demCols]/n
zipCode.aggSum$Number.of.returns<-n

zipCode.aggSum<-zipCode.aggSum[order(zipCode.aggSum$Salaries.and.wages.amount,
                                   decreasing = TRUE),]



format(object.size(zipCode.aggSum), units = 'Mb')


library(caret)
preProc<-preProcess(irs[, demCols], method = c('zv', 'nzv', 'pca'))
predict(preProc, getZip(29412))



