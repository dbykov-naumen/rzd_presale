package process

object DataProcessor {

    val PathName = "C:/Users/DByikov/Desktop/rgd/"
    val PathNameAllData = "C:/Users/DByikov/Desktop/rgd_res/"

    val FileNameToSave = "allData.xls"
    val SavedExcelMainSheetName = "Data"

    val ClstPath = "C:/Users/DByikov/Desktop/clst/allCategories/"
    val ClstNamesHardNegative: Array[String] = Array[String]("clst29.xls", "clst35.xls")
    val ClstNamesSoftNegative: Array[String] = Array[String](
        "clst27.xls", "clst28.xls", "clst34.xls", "clst35.xls",
        "clst36.xls", "clst37.xls", "clst38.xls", "clst39.xls",
        "clst43.xls", "clst46.xls", "clst47.xls")

    val StopCategoriesForNegativeCall: Array[String] = Array[String]("1.71")
    val StopWordsForNegativeCall: Array[String] = Array[String]("Благодарность")

    val AcceptableCategoriesForPositiveCall: Array[String] = Array[String]("1.71", "1.72", "1.73", "1.74")
    val AcceptableWordsForPositiveCall: Array[String] = Array[String]("Благодарность")

    val KeyWordsAffiliatesToReplace: Array[String] = Array[String]("?", "+")

    val PercentileUnresolvedAbsolute = 0.07F
    val PercentileInProgressRelativeToUnresolved = 0.15F

    var allData: Seq[FullInfo] = new Array[FullInfo](0)

    def process(): Unit = {

        val dataExtractor = new DataExtractor

        allData = dataExtractor.extractFullInfoFromPath(PathName)

        ClstNamesHardNegative.foreach { fileName =>
            dataExtractor.extractRatingCallsFromExcel(
                excelFullFileName = ClstPath + fileName,
                stopCategories = StopCategoriesForNegativeCall,
                stopWords = StopWordsForNegativeCall,
                status = RatingStatus.HardNegative,
                resultToFill = allData)
        }
        ClstNamesSoftNegative.foreach { fileName =>
            dataExtractor.extractRatingCallsFromExcel(
                excelFullFileName = ClstPath + fileName,
                stopCategories = StopCategoriesForNegativeCall,
                stopWords = StopWordsForNegativeCall,
                status = RatingStatus.SoftNegative,
                resultToFill = allData)
        }
        dataExtractor.extractRatingCallsFromResult(
            allData,
            AcceptableCategoriesForPositiveCall,
            AcceptableWordsForPositiveCall,
            RatingStatus.Positive
        )

        val namedCategories = dataExtractor.extractNamedCategoriesFromPath(PathName).
            groupBy(x => x._1).
            filter(x => x._1 != null.asInstanceOf[String] && x._1 != "unknown" && x._1 != "").
            map(x => x._1 -> x._2.head._2)
        allData.map { fullInfo =>
            fullInfo.category = if (namedCategories.contains(fullInfo.category)) namedCategories(fullInfo.category)
            else "Без категории"
            fullInfo
        }

        allData.map { fullInfo =>
            if (RatingStatus.Unknown == fullInfo.ratingStatus) fullInfo.ratingStatus = RatingStatus.Neutral
            fullInfo
        }

        val responsiblesSeed = allData.filter(_.responsible != "").map(_.responsible).distinct
        val dataSim = new DataSim(allData)
        dataSim.simulateResponsibles(responsiblesSeed)

        dataSim.simulateResolvingStatuses(
            percentileUnresolved = PercentileUnresolvedAbsolute,
            percentileInProgress = PercentileUnresolvedAbsolute * PercentileInProgressRelativeToUnresolved
        )

        val affiliatesSeed =
            dataExtractor.extractAffiliatesSeedFromResult(KeyWordsAffiliatesToReplace, allData)

        dataSim.simulateAffiliates(KeyWordsAffiliatesToReplace, affiliatesSeed)

        val trainCodesSeeds = dataSim.simulateTrainsCodes(responsiblesSeed.length * 10, affiliatesSeed)

        dataSim.simulateTrainStaffsCodes(3 -> 6, trainCodesSeeds)

        dataSim.simulateUUIDs()
    }

    def saveAllDataExcel(excelFullFileName: String): Unit = {

        val excelProcessor = new ExcelProcessor(excelFullFileName)

        val data2Write = new Array[Seq[String]](allData.length)
        var i = 0
        allData.foreach { fullInfo =>
            val dataStr = new Array[String](15)
            dataStr(0) = fullInfo.uuid
            dataStr(1) = fullInfo.date
            dataStr(2) = fullInfo.source
            dataStr(3) = fullInfo.cipher
            dataStr(4) = fullInfo.call
            dataStr(5) = fullInfo.subdivision
            dataStr(6) = fullInfo.addressed
            dataStr(7) = fullInfo.affiliate
            dataStr(8) = fullInfo.responsible
            dataStr(9) = fullInfo.comment
            dataStr(10) = fullInfo.category
            dataStr(11) = fullInfo.trainCode
            dataStr(12) = fullInfo.staffCode
            dataStr(13) = fullInfo.ratingStatus.toString
            dataStr(14) = fullInfo.resolvingStatus.toString

            data2Write(i) = dataStr

            i += 1
        }

        val sheetName = SavedExcelMainSheetName
        val oxTableLabels = Array[String](
            FullInfo.CaseArgumentsAsString.uuid,
            FullInfo.CaseArgumentsAsString.date, FullInfo.CaseArgumentsAsString.source, FullInfo.CaseArgumentsAsString.cipher,
            FullInfo.CaseArgumentsAsString.call, FullInfo.CaseArgumentsAsString.subdivision, FullInfo.CaseArgumentsAsString.addressed,
            FullInfo.CaseArgumentsAsString.affiliate, FullInfo.CaseArgumentsAsString.responsible, FullInfo.CaseArgumentsAsString.comment,
            FullInfo.CaseArgumentsAsString.category, FullInfo.CaseArgumentsAsString.trainCode, FullInfo.CaseArgumentsAsString.staffCode,
            FullInfo.CaseArgumentsAsString.ratingStatus.toString, FullInfo.CaseArgumentsAsString.resolvingStatus.toString
        )

        excelProcessor.writeTable(
            sheetName = sheetName,
            oxTableLabels = oxTableLabels,
            oyTableLabels = new Array[String](0),
            data = data2Write
        )
    }

    def main(args: Array[String]): Unit = {

        process()
        saveAllDataExcel(PathNameAllData + FileNameToSave)
    }
}
