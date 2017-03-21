package represent

import process.RatingStatus.RatingStatus
import process.ResolvingStatus.ResolvingStatus
import process._

import scala.util.Random

case class RatingStatusesByMonths(
                                     month: Int,
                                     totalHardNegative: Int,
                                     totalSoftNegative: Int,
                                     totalNeutral: Int,
                                     totalPositive: Int
                                 )

case class RatingStatusesByTrainCodes(
                                         trainCode: String,
                                         totalHardNegative: Int,
                                         totalSoftNegative: Int,
                                         totalNeutral: Int,
                                         totalPositive: Int
                                     )

case class RatingStatusesByStaffCodes(
                                         staffCode: String,
                                         totalHardNegative: Int,
                                         totalSoftNegative: Int,
                                         totalNeutral: Int,
                                         totalPositive: Int
                                     )

case class RatingStatusesByAffiliates(
                                         affiliate: String,
                                         totalHardNegative: Int,
                                         totalSoftNegative: Int,
                                         totalNeutral: Int,
                                         totalPositive: Int
                                     )

case class ResolvingStatusesByMonths(
                                        month: Int,
                                        totalResolved: Int,
                                        totalUnresolved: Int,
                                        totalInProgress: Int
                                    )

case class ResolvingStatusesByResponsibles(
                                              responsible: String,
                                              totalResolved: Int,
                                              totalUnresolved: Int,
                                              totalInProgress: Int
                                          )

case class ResolvingStatusesByAffiliates(
                                            affiliate: String,
                                            totalResolved: Int,
                                            totalUnresolved: Int,
                                            totalInProgress: Int
                                        )

case class ResolvingStatusesByCategories(
                                            category: String,
                                            totalResolved: Int,
                                            totalUnresolved: Int,
                                            totalInProgress: Int
                                        )

case class ResolvingStatusesBySources(
                                         source: String,
                                         totalResolved: Int,
                                         totalUnresolved: Int,
                                         totalInProgress: Int
                                     )

case class ResponsibleWithRate(
                                  responsible: String,
                                  affiliate: String,
                                  numberResolved: Int,
                                  avgTimeResolvingMinutes: Int
                              )

object DataPresentServiceExcel {

    private var allData: Seq[FullInfo] = new Array[FullInfo](0)

    private def retrieveAllData(excelFullFileName: String): Unit = {

        val excelProcessor = new ExcelProcessor(excelFullFileName)

        val uuids = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.uuid,
            numberValuesAsFlt = true,
            rowId = 0,
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true
        )

        val dates = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.date,
            numberValuesAsFlt = true,
            rowId = excelProcessor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = excelProcessor.getCountOfNonEmptyRows.getOrElse(uuids.size)
        )

        val sources = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.source,
            numberValuesAsFlt = true,
            rowId = excelProcessor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = excelProcessor.getCountOfNonEmptyRows.getOrElse(uuids.size)
        )

        val ciphers = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.cipher,
            numberValuesAsFlt = true,
            rowId = excelProcessor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = excelProcessor.getCountOfNonEmptyRows.getOrElse(uuids.size)
        )

        val calls = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.call,
            numberValuesAsFlt = true,
            rowId = excelProcessor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = excelProcessor.getCountOfNonEmptyRows.getOrElse(uuids.size)
        )

        val subdivisions = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.subdivision,
            numberValuesAsFlt = true,
            rowId = excelProcessor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = excelProcessor.getCountOfNonEmptyRows.getOrElse(uuids.size)
        )

        val addresseds = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.addressed,
            numberValuesAsFlt = true,
            rowId = excelProcessor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = excelProcessor.getCountOfNonEmptyRows.getOrElse(uuids.size)
        )

        val affiliates = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.affiliate,
            numberValuesAsFlt = true,
            rowId = excelProcessor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = excelProcessor.getCountOfNonEmptyRows.getOrElse(uuids.size)
        )

        val responsibles = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.responsible,
            numberValuesAsFlt = true,
            rowId = excelProcessor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = excelProcessor.getCountOfNonEmptyRows.getOrElse(uuids.size)
        )

        val comments = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.comment,
            numberValuesAsFlt = true,
            rowId = excelProcessor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = excelProcessor.getCountOfNonEmptyRows.getOrElse(uuids.size)
        )

        val categories = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.category,
            numberValuesAsFlt = true,
            rowId = excelProcessor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = excelProcessor.getCountOfNonEmptyRows.getOrElse(uuids.size)
        )

        val trainCodes = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.trainCode,
            numberValuesAsFlt = true,
            rowId = excelProcessor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = excelProcessor.getCountOfNonEmptyRows.getOrElse(uuids.size)
        )

        val staffCodes = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.staffCode,
            numberValuesAsFlt = true,
            rowId = excelProcessor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = excelProcessor.getCountOfNonEmptyRows.getOrElse(uuids.size)
        )

        val ratingStatuses = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.ratingStatus.toString,
            numberValuesAsFlt = true,
            rowId = excelProcessor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = excelProcessor.getCountOfNonEmptyRows.getOrElse(uuids.size)
        )

        val resolvingStatuses = excelProcessor.readDataFromSheet(
            sheetName = DataProcessor.SavedExcelMainSheetName,
            colName = FullInfo.CaseArgumentsAsString.resolvingStatus.toString,
            numberValuesAsFlt = true,
            rowId = excelProcessor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = excelProcessor.getCountOfNonEmptyRows.getOrElse(uuids.size)
        )

        val allDataLoc = new Array[FullInfo](uuids.length)
        var i = 0
        while (i < uuids.length) {
            val fullInfo = FullInfo(
                uuid = uuids(i),
                date = dates(i),
                source = sources(i),
                cipher = ciphers(i),
                call = calls(i),
                subdivision = subdivisions(i),
                addressed = addresseds(i),
                affiliate = affiliates(i),
                responsible = responsibles(i),
                comment = comments(i),
                category = categories(i),
                trainCode = trainCodes(i),
                staffCode = staffCodes(i),
                ratingStatus = RatingStatus(ratingStatuses(i)),
                resolvingStatus = ResolvingStatus(resolvingStatuses(i))
            )
            allDataLoc(i) = fullInfo
            i += 1
        }
        allData = allDataLoc
    }

    def main(args: Array[String]): Unit = {

        retrieveAllData(DataProcessor.PathNameAllData + DataProcessor.FileNameToSave)

        val dataPresentServiceExcel = new DataPresentServiceExcel(DataProcessor.PathNameAllData + DataProcessor.FileNameToSave)

        val ratingStatusesByMonths = dataPresentServiceExcel.ratingStatusesByMonths(
            Option("Ю-ВОСТ")
        )
        val resolvingStatusesByMonths = dataPresentServiceExcel.resolvingStatusesByMonths(
            Option("З-СИБ")
        )

        println(ratingStatusesByMonths)
        println(resolvingStatusesByMonths)
    }
}

class DataPresentServiceExcel(excelFullFileName: String) {

    val processor = new ExcelProcessor(excelFullFileName)

    {
        if (DataPresentServiceExcel.allData.isEmpty)
            DataPresentServiceExcel.retrieveAllData(excelFullFileName)
    }

    private def countRatingStatus(record: Map[RatingStatus, Int]): (Int, Int, Int, Int) = {

        Tuple4(
            if (record.contains(RatingStatus.HardNegative)) record(RatingStatus.HardNegative)
            else 0,
            if (record.contains(RatingStatus.SoftNegative)) record(RatingStatus.SoftNegative)
            else 0,
            if (record.contains(RatingStatus.Neutral)) record(RatingStatus.Neutral)
            else 0,
            if (record.contains(RatingStatus.Positive)) record(RatingStatus.Positive)
            else 0
        )
    }

    private def countResolvingStatus(record: Map[ResolvingStatus, Int]): (Int, Int, Int) = {

        Tuple3(
            if (record.contains(ResolvingStatus.Resolved)) record(ResolvingStatus.Resolved)
            else 0,
            if (record.contains(ResolvingStatus.Unresolved)) record(ResolvingStatus.Unresolved)
            else 0,
            if (record.contains(ResolvingStatus.InProgress)) record(ResolvingStatus.InProgress)
            else 0
        )
    }

    def ratingStatusesByMonths(
                                  affiliate: Option[String] = None
                              ): Seq[RatingStatusesByMonths] = {

        if (DataPresentServiceExcel.allData.isEmpty)
            DataPresentServiceExcel.retrieveAllData(excelFullFileName)

        val calls = affiliate.
            map(x => DataPresentServiceExcel.allData.filter(xx => xx.affiliate.toUpperCase == x.toUpperCase())).
            getOrElse(DataPresentServiceExcel.allData).
            map(x => x.date -> x.ratingStatus).
            filter(x => null != x._1 && x._1.length >= 5).
            groupBy(x => x._1.substring(3, 5).toInt).
            map(x => x._1 -> x._2.unzip._2.groupBy(xx => xx).map(xx => xx._1 -> xx._2.length))


        val res = calls.map { x =>
            val counts = countRatingStatus(x._2)
            RatingStatusesByMonths(
                month = x._1,
                totalHardNegative = counts._1,
                totalSoftNegative = counts._2,
                totalNeutral = counts._3,
                totalPositive = counts._4
            )
        }.toSeq.sortBy(_.month)

        res
    }

    def ratingStatusesByTrainCodes(
                                      trainCode: String,
                                      affiliate: Option[String] = None
                                  ): Seq[RatingStatusesByTrainCodes] = {

        val calls = getCalls(
            uuid = None,
            ratingStatus = None,
            affiliate = affiliate,
            trainCode = Option(trainCode)
        )

        calls.groupBy(x => x.trainCode).
            map(x => x._1 -> x._2.groupBy(xx => xx.ratingStatus).map(xx => xx._1 -> xx._2.length)).
            map { x =>
                val counts = countRatingStatus(x._2)
                RatingStatusesByTrainCodes(
                    trainCode = x._1,
                    totalHardNegative = counts._1,
                    totalSoftNegative = counts._2,
                    totalNeutral = counts._3,
                    totalPositive = counts._4
                )
            }.
            toSeq
    }

    def ratingStatusesByStaffCodes(
                                      staffCode: String,
                                      affiliate: Option[String] = None
                                  ): Seq[RatingStatusesByStaffCodes] = {

        val calls = getCalls(
            uuid = None,
            ratingStatus = None,
            affiliate = affiliate,
            trainCode = None,
            staffCode = Option(staffCode)
        )

        calls.groupBy(x => x.staffCode).
            map(x => x._1 -> x._2.groupBy(xx => xx.ratingStatus).map(xx => xx._1 -> xx._2.length)).
            map { x =>
                val counts = countRatingStatus(x._2)
                RatingStatusesByStaffCodes(
                    staffCode = x._1,
                    totalHardNegative = counts._1,
                    totalSoftNegative = counts._2,
                    totalNeutral = counts._3,
                    totalPositive = counts._4
                )
            }.
            toSeq
    }

    def ratingStatusesByAffiliates(): Seq[RatingStatusesByAffiliates] = {

        val calls = getCalls()

        calls.groupBy(x => x.affiliate).
            map(x => x._1 -> x._2.groupBy(xx => xx.ratingStatus).map(xx => xx._1 -> xx._2.length)).
            map { x =>
                val counts = countRatingStatus(x._2)
                RatingStatusesByAffiliates(
                    affiliate = x._1,
                    totalHardNegative = counts._1,
                    totalSoftNegative = counts._2,
                    totalNeutral = counts._3,
                    totalPositive = counts._4
                )
            }.
            toSeq
    }

    def resolvingStatusesByMonths(
                                     affiliate: Option[String] = None
                                 ): Seq[ResolvingStatusesByMonths] = {

        if (DataPresentServiceExcel.allData.isEmpty)
            DataPresentServiceExcel.retrieveAllData(excelFullFileName)

        val calls = affiliate.
            map(x => DataPresentServiceExcel.allData.filter(xx => xx.affiliate.toUpperCase == x.toUpperCase())).
            getOrElse(DataPresentServiceExcel.allData).
            map(x => x.date -> x.resolvingStatus).
            filter(x => null != x._1 && x._1.length >= 5).
            groupBy(x => x._1.substring(3, 5).toInt).
            map(x => x._1 -> x._2.unzip._2.groupBy(xx => xx).map(xx => xx._1 -> xx._2.length))


        val res = calls.map { x =>
            val counts = countResolvingStatus(x._2)
            ResolvingStatusesByMonths(
                month = x._1,
                totalResolved = counts._1,
                totalUnresolved = counts._2,
                totalInProgress = counts._3
            )
        }.toSeq.sortBy(_.month)

        res
    }

    def resolvingStatusesByAffiliates(): Seq[ResolvingStatusesByAffiliates] = {

        val calls = getCalls()

        calls.groupBy(x => x.affiliate).
            map(x => x._1 -> x._2.groupBy(xx => xx.resolvingStatus).map(xx => xx._1 -> xx._2.length)).
            map { x =>
                val counts = countResolvingStatus(x._2)
                ResolvingStatusesByAffiliates(
                    affiliate = x._1,
                    totalResolved = counts._1,
                    totalUnresolved = counts._2,
                    totalInProgress = counts._3
                )
            }.
            toSeq
    }

    def resolvingStatusesByResponsibles(
                                           affiliate: Option[String] = None
                                       ): Seq[ResolvingStatusesByResponsibles] = {

        val calls = getCalls(
            uuid = None,
            ratingStatus = None,
            affiliate = affiliate
        )

        calls.groupBy(x => x.responsible).
            map(x => x._1 -> x._2.groupBy(xx => xx.resolvingStatus).map(xx => xx._1 -> xx._2.length)).
            map { x =>
                val counts = countResolvingStatus(x._2)
                ResolvingStatusesByResponsibles(
                    responsible = x._1,
                    totalResolved = counts._1,
                    totalUnresolved = counts._2,
                    totalInProgress = counts._3
                )
            }.
            toSeq
    }

    def resolvingStatusesByCategories(
                                         affiliate: Option[String] = None
                                     ): Seq[ResolvingStatusesByCategories] = {

        val calls = getCalls(
            uuid = None,
            ratingStatus = None,
            affiliate = affiliate
        )

        calls.groupBy(x => x.category).
            map(x => x._1 -> x._2.groupBy(xx => xx.resolvingStatus).map(xx => xx._1 -> xx._2.length)).
            map { x =>
                val counts = countResolvingStatus(x._2)
                ResolvingStatusesByCategories(
                    category = x._1,
                    totalResolved = counts._1,
                    totalUnresolved = counts._2,
                    totalInProgress = counts._3
                )
            }.
            toSeq
    }

    def resolvingStatusesBySources(
                                      affiliate: Option[String] = None
                                  ): Seq[ResolvingStatusesBySources] = {

        val calls = getCalls(
            uuid = None,
            ratingStatus = None,
            affiliate = affiliate
        )

        calls.groupBy(x => x.source).
            map(x => x._1 -> x._2.groupBy(xx => xx.resolvingStatus).map(xx => xx._1 -> xx._2.length)).
            map { x =>
                val counts = countResolvingStatus(x._2)
                ResolvingStatusesBySources(
                    source = x._1,
                    totalResolved = counts._1,
                    totalUnresolved = counts._2,
                    totalInProgress = counts._3
                )
            }.
            toSeq
    }

    def timelineNonresolvedCalls(
                                    numberOfTop: Int,
                                    oldestNotNewest: Boolean,
                                    affiliate: Option[String] = None
                                ): Seq[FullInfo] = {

        val calls = getCalls(
            uuid = None,
            ratingStatus = None,
            affiliate = affiliate
        )

        val oldestCalls = calls.
            filter(fullInfo => null != fullInfo.date && fullInfo.date.length >= 5 &&
                fullInfo.resolvingStatus != ResolvingStatus.Resolved).
            groupBy(fullInfo =>
                (fullInfo.date.substring(3, 5).toInt + fullInfo.date.substring(0, 2).toInt) * {
                    if (oldestNotNewest) 1
                    else -1
                }
            ).
            toSeq.
            flatMap(x => x._2)

        oldestCalls.
            splitAt {
                if (oldestCalls.length >= numberOfTop) numberOfTop
                else oldestCalls.length
            }._1
    }

    def responsiblesByResolvedLastMonth(
                                           numberOfTop: Int,
                                           bestsNotWorsts: Boolean,
                                           resolvingRateRangeMinutes: (Int, Int),
                                           affiliate: Option[String] = None
                                       ): Seq[ResponsibleWithRate] = {

        val calls = getCalls(
            uuid = None,
            ratingStatus = None,
            affiliate = affiliate
        )

        val callsFiltered = calls.
            filter(x => x.resolvingStatus == ResolvingStatus.Resolved).
            filter(x => null != x.date && x.date.length >= 5).
            groupBy(x => x.responsible).
            map { x =>
                val byMonth = x._2.groupBy(xx => xx.date.substring(3, 5).toInt)
                val numberLastMonth = byMonth.maxBy(_._1)._2.length
                x._1 -> numberLastMonth
            }.
            toSeq.
            sortBy(x => x._2 * {
                if (bestsNotWorsts) -1 else 1
            }).
            map(x => ResponsibleWithRate(
                responsible = x._1,
                affiliate = affiliate.getOrElse(""),
                numberResolved = x._2,
                avgTimeResolvingMinutes = new Random(x._2).nextInt(resolvingRateRangeMinutes._2 - resolvingRateRangeMinutes._1) + resolvingRateRangeMinutes._1
            ))

        callsFiltered.
            splitAt {
                if (callsFiltered.length >= numberOfTop) numberOfTop
                else callsFiltered.length
            }._1
    }

    def getCalls(
                    uuid: Option[String] = None,
                    ratingStatus: Option[String] = None,
                    affiliate: Option[String] = None,
                    trainCode: Option[String] = None,
                    staffCode: Option[String] = None,
                    responsible: Option[String] = None,
                    category: Option[String] = None,
                    source: Option[String] = None
                ): Seq[FullInfo] = {

        if (DataPresentServiceExcel.allData.isEmpty) {
            DataPresentServiceExcel.retrieveAllData(excelFullFileName)
        }

        val calls = DataPresentServiceExcel.allData

        var res = uuid.map(x => calls.filter(xx => xx.uuid.toUpperCase == x.toUpperCase)).getOrElse(calls)
        res = ratingStatus.map(x => res.filter(xx => xx.ratingStatus.toString.toUpperCase == x.toUpperCase)).getOrElse(res)
        res = affiliate.map(x => res.filter(xx => xx.affiliate.toUpperCase == x.toUpperCase)).getOrElse(res)
        res = trainCode.map(x => res.filter(xx => xx.trainCode.toUpperCase == x.toUpperCase)).getOrElse(res)
        res = staffCode.map(x => res.filter(xx => xx.staffCode.toUpperCase == x.toUpperCase)).getOrElse(res)
        res = responsible.map(x => res.filter(xx => xx.responsible.toUpperCase == x.toUpperCase)).getOrElse(res)
        res = category.map(x => res.filter(xx => xx.category.toUpperCase == x.toUpperCase)).getOrElse(res)
        res = source.map(x => res.filter(xx => xx.source.toUpperCase == x.toUpperCase)).getOrElse(res)

        res
    }

}
