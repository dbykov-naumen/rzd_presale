package process

import java.io.File

import process.RatingStatus.RatingStatus
import process.ResolvingStatus.ResolvingStatus

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object RatingStatus extends Enumeration {
    type RatingStatus = Value

    def apply(stringValue : String): RatingStatus = {
        stringValue match {
            case "HardNegative" => HardNegative
            case "SoftNegative" => SoftNegative
            case "Neutral" => Neutral
            case "Positive" => Positive
            case _ => Unknown
        }
    }

    val HardNegative = Value("HardNegative")
    val SoftNegative = Value("SoftNegative")
    val Neutral = Value("Neutral")
    val Positive = Value("Positive")
    val Unknown = Value("Unknown")

    val self = Value("RatingStatus")
}

object ResolvingStatus extends Enumeration {
    type ResolvingStatus = Value

    def apply(stringValue : String): ResolvingStatus = {
        stringValue match {
            case "Resolved" => Resolved
            case "Unresolved" => Unresolved
            case "InProgress" => InProgress
            case _ => Unknown
        }
    }

    val Resolved = Value("Resolved")
    val Unresolved = Value("Unresolved")
    val InProgress = Value("InProgress")
    val Unknown = Value("Unknown")

    val self = Value("ResolvingStatus")
}

case class FullInfo(
                       var uuid: String,
                       date: String,
                       source: String,
                       cipher: String,
                       call: String,
                       subdivision: String,
                       addressed: String,
                       var affiliate: String,
                       var responsible: String,
                       comment: String,
                       var category: String,
                       var trainCode: String,
                       var staffCode: String,
                       var ratingStatus: RatingStatus,
                       var resolvingStatus: ResolvingStatus
                   )

object FullInfo {

    val CaseArgumentsAsString =
        FullInfo(
            "uuid", "date", "source", "cipher", "call", "subdivision", "addressed", "affiliate", "responsible",
            "comment", "category", "trainCode", "staffCode", RatingStatus.self, ResolvingStatus.self
        )
}

object DataExtractor {

    val SheetNameRegistryInput = "Реестр"
    val ColNameCipherInput = "Шифр"
    val ColNameCallInput = "Описание обращения"
    val ColNameAffiliateInput = "Филиал"
    val ColOriginSubdivisionInput = "Ответственное подразделение"
    val ColNameAddressedInput0 = "Данные обратившегося"
    val ColNameAddressedInput1 = "Данные заявителя"
    val ColOriginResponsibleInput = "Ответственный"
    val ColOriginCommentInput = "Комментар"
    val ColNameCategoryInput = "Пункт классификатора"

    val SheetNameClstInput = "Обращения"
    val ColNameClstCallInput = "Обращение"
    val ColNameClstCategoryInput = "Тематика"

    val SheetNameCategoriesInput = "Тематика"
    val ColNameCategoryParagraphInput = "Пункт"
    val ColNameCategoryContentInput = "Тематика обращений"

    val MarkerDateRow = Option(Array[String]("Ежедневный учет", "Ежедневный учёт").toSeq)

    val DateRegex = new Regex(
        "(?:(?:31(\\/|-|\\.)(?:0?[13578]|1[02]))\\1|(?:(?:29|30)" +
            "(\\/|-|\\.)(?:0?[1,3-9]|1[0-2])\\2))(?:(?:1[6-9]|[2-9]\\d)?" +
            "\\d{2})$|^(?:29(\\/|-|\\.)0?2\\3(?:(?:(?:1[6-9]|[2-9]\\d)?(?" +
            ":0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))" +
            "|(?:0?[1-9]|1\\d|2[0-8])(\\/|-|\\.)(?:(?:0?[1-9])|(?:1[0-2]))\\4(?:(?:1[6-9]|[2-9]\\d)?\\d{2})"
    )

    val SourcesTags: Seq[String] = Array[String](
        "Горячая линия РЖД",
        "Общественная приёмная М.П. Акулова",
        "эл. почта ГенДиректора",
        "Общественная приемная РЖД"
    ).toSeq

    val StartRowIdInput = 2
    val MaxCellsNumberToSeekInput = 15
}

class DataExtractor {

    def extractFullInfoFromExcel(
                                    excelFullFileName: String,
                                    skipWithEmptyDate: Boolean = true
                                ): Seq[FullInfo] = {

        val processor = new ExcelProcessor(excelFullFileName)
        val sheetNames = processor.getSheetNamesThatStartsWith(DataExtractor.SheetNameRegistryInput)
        val result = new ArrayBuffer[FullInfo]()

        sheetNames.foreach { sheetName =>

            val date = processor.getContentByRegex(
                sheetName = sheetName,
                possibleIdRows = Array[Int](0, 1, 2, 3),
                possbleIdCells = Array[Int](0, 1, 2, 3),
                regex = DataExtractor.DateRegex,
                startsWith = DataExtractor.MarkerDateRow
            )

            if (date.nonEmpty || !skipWithEmptyDate) {

                val dateStr = if (date.isEmpty) "" else date.head

                val source = {
                    var res = ""
                    DataExtractor.SourcesTags.foreach { tag => if (excelFullFileName.contains(tag)) res = tag }
                    res
                }

                val ciphers = processor.readDataFromSheet(
                    sheetName = sheetName,
                    colName = DataExtractor.ColNameCipherInput,
                    numberValuesAsFlt = true,
                    rowId = DataExtractor.StartRowIdInput,
                    findNextRow = true,
                    maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
                    parseAlwaysAsString = true
                )

                val calls = processor.readDataFromSheet(
                    sheetName = sheetName,
                    colName = DataExtractor.ColNameCallInput,
                    numberValuesAsFlt = true,
                    rowId = processor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
                    findNextRow = true,
                    maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
                    parseAlwaysAsString = true,
                    rowCount = processor.getCountOfNonEmptyRows.getOrElse(ciphers.size)
                )

                val affiliates = processor.readDataFromSheet(
                    sheetName = sheetName,
                    colName = DataExtractor.ColNameAffiliateInput,
                    numberValuesAsFlt = true,
                    rowId = processor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
                    findNextRow = true,
                    maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
                    parseAlwaysAsString = true,
                    rowCount = processor.getCountOfNonEmptyRows.getOrElse(ciphers.size)
                )

                val subdivisions = processor.readDataFromSheet(
                    sheetName = sheetName,
                    colName = DataExtractor.ColOriginSubdivisionInput,
                    numberValuesAsFlt = true,
                    rowId = processor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
                    findNextRow = true,
                    maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
                    parseAlwaysAsString = true,
                    rowCount = processor.getCountOfNonEmptyRows.getOrElse(ciphers.size),
                    startsWithColName = true
                )

                var addresseds = processor.readDataFromSheet(
                    sheetName = sheetName,
                    colName = DataExtractor.ColNameAddressedInput0,
                    numberValuesAsFlt = true,
                    rowId = processor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
                    findNextRow = true,
                    maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
                    parseAlwaysAsString = true,
                    rowCount = processor.getCountOfNonEmptyRows.getOrElse(ciphers.size),
                    startsWithColName = false,
                    showMessage = false
                )
                if (addresseds.isEmpty) {
                    addresseds = processor.readDataFromSheet(
                        sheetName = sheetName,
                        colName = DataExtractor.ColNameAddressedInput1,
                        numberValuesAsFlt = true,
                        rowId = processor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
                        findNextRow = true,
                        maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
                        parseAlwaysAsString = true,
                        rowCount = processor.getCountOfNonEmptyRows.getOrElse(ciphers.size)
                    )
                    if (addresseds.isEmpty) addresseds = Array.fill[String](ciphers.size)("")
                }

                var responsibles = processor.readDataFromSheet(
                    sheetName = sheetName,
                    colName = DataExtractor.ColOriginResponsibleInput,
                    numberValuesAsFlt = true,
                    rowId = processor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
                    findNextRow = true,
                    maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
                    parseAlwaysAsString = true,
                    rowCount = processor.getCountOfNonEmptyRows.getOrElse(ciphers.size),
                    startsWithColName = true,
                    showMessage = false
                )
                if (responsibles.isEmpty) responsibles = Array.fill[String](ciphers.size)("")

                var comments = processor.readDataFromSheet(
                    sheetName = sheetName,
                    colName = DataExtractor.ColOriginCommentInput,
                    numberValuesAsFlt = true,
                    rowId = processor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
                    findNextRow = true,
                    maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
                    parseAlwaysAsString = true,
                    rowCount = processor.getCountOfNonEmptyRows.getOrElse(ciphers.size),
                    startsWithColName = true,
                    showMessage = false
                )
                if (comments.isEmpty) comments = Array.fill[String](ciphers.size)("")

                val categories = processor.readDataFromSheet(
                    sheetName = sheetName,
                    colName = DataExtractor.ColNameCategoryInput,
                    numberValuesAsFlt = true,
                    rowId = processor.getIndexOfStartRow.getOrElse(DataExtractor.StartRowIdInput),
                    findNextRow = true,
                    maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
                    parseAlwaysAsString = true,
                    rowCount = processor.getCountOfNonEmptyRows.getOrElse(ciphers.size)
                )

                assert(
                    ciphers.length == calls.length &&
                        calls.length == affiliates.length &&
                        affiliates.length == addresseds.length &&
                        addresseds.length == subdivisions.length &&
                        subdivisions.length == categories.length,
                    "Wrong data extracted from excel: different fields lengths. File: " + excelFullFileName
                )

                val res = new Array[FullInfo](calls.length)
                var i = 0
                while (i < calls.length) {
                    res(i) = FullInfo(
                        uuid = "unset",
                        date = dateStr,
                        source = source,
                        cipher = ciphers(i),
                        call = calls(i),
                        subdivision = subdivisions(i),
                        addressed = addresseds(i),
                        affiliate = affiliates(i),
                        responsible = responsibles(i),
                        comment = comments(i),
                        category = categories(i),
                        trainCode = "",
                        staffCode = "",
                        ratingStatus = RatingStatus.Unknown,
                        resolvingStatus = ResolvingStatus.Unknown
                    )
                    i += 1
                }

                result ++= res

            }
        }

        result
    }

    def extractNamedCategoriesFromExcel(excelFullFileName: String): Seq[(String, String)] = {

        val processor = new ExcelProcessor(excelFullFileName)

        try {
            val categoriesParagraphs = processor.readDataFromSheet(
                sheetName = DataExtractor.SheetNameCategoriesInput,
                colName = DataExtractor.ColNameCategoryParagraphInput,
                numberValuesAsFlt = true,
                rowId = 0,
                findNextRow = true,
                maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
                parseAlwaysAsString = true
            ).map(x => if (null != x && "" != x) x else "Без категории")

            val categoriesContents = processor.readDataFromSheet(
                sheetName = DataExtractor.SheetNameCategoriesInput,
                colName = DataExtractor.ColNameCategoryContentInput,
                numberValuesAsFlt = true,
                rowId = processor.getIndexOfStartRow.getOrElse(0),
                findNextRow = true,
                maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
                parseAlwaysAsString = true,
                rowCount = processor.getCountOfNonEmptyRows.getOrElse(categoriesParagraphs.size)
            ).map(x => if (null != x && "" != x) x else "Без категории")

            (categoriesParagraphs zip categoriesContents).
                groupBy(x => x._1).
                map(x => x._1 -> x._2.head._2).
                toSeq
        } catch {
            case e: Exception => new Array[(String, String)](0)
        }
    }

    def extractNamedCategoriesFromPath(
                                          path: String,
                                          includeSubPaths: Boolean = true
                                      ): Seq[(String, String)] = {

        val filesList = new File(path).listFiles()
        val res = new ArrayBuffer[(String, String)]()
        filesList.foreach { file =>
            if (file.isDirectory) {
                res ++= extractNamedCategoriesFromPath(file.getAbsolutePath)
            } else if (file.isFile) {
                val fileName = file.getAbsolutePath
                if (fileName.endsWith(".xls") || fileName.endsWith(".xlsx")) {
                    res ++= extractNamedCategoriesFromExcel(fileName)
                }
            }
        }
        res
    }

    def extractFullInfoFromPath(
                                   path: String,
                                   includeSubPaths: Boolean = true
                               ): Seq[FullInfo] = {

        val filesList = new File(path).listFiles()
        val res = new ArrayBuffer[FullInfo]()
        filesList.foreach { file =>
            if (file.isDirectory) {
                res ++= extractFullInfoFromPath(file.getAbsolutePath)
            } else if (file.isFile) {
                val fileName = file.getAbsolutePath
                if (fileName.endsWith(".xls") || fileName.endsWith(".xlsx")) {
                    res ++= extractFullInfoFromExcel(fileName, skipWithEmptyDate = false)
                }
            }
        }
        res
    }

    def extractRatingCallsFromExcel(
                                       excelFullFileName: String,
                                       stopCategories: Seq[String],
                                       stopWords: Seq[String],
                                       status: RatingStatus,
                                       resultToFill: Seq[FullInfo]
                                   ): Unit = {

        val processor = new ExcelProcessor(excelFullFileName)

        val calls = processor.readDataFromSheet(
            sheetName = DataExtractor.SheetNameClstInput,
            colName = DataExtractor.ColNameClstCallInput,
            numberValuesAsFlt = true,
            rowId = 0,
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true
        )

        val categories = processor.readDataFromSheet(
            sheetName = DataExtractor.SheetNameClstInput,
            colName = DataExtractor.ColNameClstCategoryInput,
            numberValuesAsFlt = true,
            rowId = processor.getIndexOfStartRow.getOrElse(0),
            findNextRow = true,
            maxCellsNumber = DataExtractor.MaxCellsNumberToSeekInput,
            parseAlwaysAsString = true,
            rowCount = processor.getCountOfNonEmptyRows.getOrElse(calls.size)
        ).map(x => if (null != x && "" != x) x else "Без категории")

        assert(calls.length == categories.length,
            "Wrong data extracted from excel: different fields lengths. File: " + excelFullFileName
        )

        val callsWithCategories = calls.zip(categories).filter { x =>
            var acceptableCall = true
            var acceptableCategory = true
            stopWords.foreach(word => acceptableCall = acceptableCall && !x._1.toUpperCase.contains(word.toUpperCase))
            stopCategories.foreach(cat => acceptableCategory = acceptableCategory && !x._2.contains(cat))
            acceptableCategory && acceptableCall
        }.toMap

        resultToFill.map { fullInfo: FullInfo =>
            if (
                RatingStatus.Unknown == fullInfo.ratingStatus &&
                    callsWithCategories.contains(fullInfo.call) &&
                    fullInfo.category.contains(callsWithCategories(fullInfo.call))) {
                fullInfo.ratingStatus = status
            }
            fullInfo
        }
    }

    def extractRatingCallsFromResult(
                                        resultFromFillToFill: Seq[FullInfo],
                                        acceptableCategories: Seq[String],
                                        acceptableWords: Seq[String],
                                        status: RatingStatus
                                    ): Unit = {

        resultFromFillToFill.foreach { fullInfo =>
            if (
                RatingStatus.Unknown == fullInfo.ratingStatus && ({
                    var res = false
                    acceptableCategories.foreach { cat => res = res || fullInfo.category.startsWith(cat) }
                    res
                } || {
                    var res = false
                    acceptableWords.foreach { word => res = res || fullInfo.call.toUpperCase.contains(word.toUpperCase) }
                    res
                })
            ) {
                fullInfo.ratingStatus = status
            }
            fullInfo
        }
    }

    def extractRatingCallsFromPath(
                                      path: String,
                                      stopCategories: Seq[String],
                                      stopWords: Seq[String],
                                      status: RatingStatus,
                                      resultToFill: Seq[FullInfo],
                                      includeSubPaths: Boolean = true
                                  ): Unit = {

        val filesList = new File(path).listFiles()
        filesList.foreach { file =>
            if (file.isDirectory) {
                extractRatingCallsFromPath(
                    path = path,
                    stopCategories = stopCategories,
                    stopWords = stopWords,
                    status = status,
                    resultToFill = resultToFill,
                    includeSubPaths = includeSubPaths
                )
            } else if (file.isFile) {
                val fileName = file.getAbsolutePath
                if (fileName.endsWith(".xls") || fileName.endsWith(".xlsx")) {
                    extractRatingCallsFromExcel(
                        excelFullFileName = fileName,
                        stopCategories = stopCategories,
                        stopWords = stopWords,
                        status = status,
                        resultToFill = resultToFill
                    )
                }
            }
        }

    }

    def extractAffiliatesSeedFromResult(
                                           keyAffiliateWords: Seq[String],
                                           resultFromExtracted: Seq[FullInfo]
                                       ): Seq[String] = {

        resultFromExtracted.
            map(_.affiliate).
            filter(affiliate => null != affiliate &&
                affiliate != "" && {
                var res = true
                keyAffiliateWords.foreach(word => res = res && !affiliate.contains(word))
                res
            }
            ).
            distinct
    }

}
