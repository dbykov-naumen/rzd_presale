package process

import java.io.{File, FileInputStream, FileOutputStream}

import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.ss.usermodel.Cell
import org.apache.poi.xssf.usermodel.XSSFWorkbook

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

class ExcelProcessor(excelFullFileName: String) {

    private lazy val fis = new FileInputStream(excelFullFileName)
    private lazy val workbookIn = if (excelFullFileName.endsWith(".xls")) new HSSFWorkbook(fis) else new XSSFWorkbook(fis)
    private lazy val workbookOut = if (excelFullFileName.endsWith(".xls")) new HSSFWorkbook() else new XSSFWorkbook()

    private var indexOfStartRow: Option[Int] = None
    private var countOfNonEmptyRows: Option[Int] = None

    require(excelFullFileName.endsWith(".xls") || excelFullFileName.endsWith(".xlsx"), "Error: input may be only \"*.xls\" or \"*.xlsx\" file")

    def getIndexOfStartRow: Option[Int] = indexOfStartRow

    def getCountOfNonEmptyRows: Option[Int] = countOfNonEmptyRows

    def getSheetNamesThatStartsWith(startsWithStr: String): Seq[String] = {

        val res = new ArrayBuffer[String]()

        val numberOfSheets = workbookIn.getNumberOfSheets
        var i = 0
        while (i < numberOfSheets) {
            val name = workbookIn.getSheetName(i)
            if (name.startsWith(startsWithStr)) {
                res += name
            }
            i += 1
        }

        res
    }

    def getContentByRegex(sheetName: String,
                          possibleIdRows: Seq[Int],
                          possbleIdCells: Seq[Int],
                          regex: Regex,
                          startsWith: Option[Seq[String]] = None
                         ): Seq[String] = {

        val sheet = workbookIn.getSheet(sheetName)
        if (null == sheet) throw new IllegalArgumentException("Error: workbook does not contain the sheet named as \"" + sheetName + "\" File: " + excelFullFileName)

        val res = new ArrayBuffer[String]()
        possibleIdRows.foreach { iRow =>
            val row = sheet.getRow(iRow)
            if (null != row) possbleIdCells.foreach { iCell =>
                val cell = row.getCell(iCell)
                if (null != cell && Cell.CELL_TYPE_STRING == cell.getCellType) {
                    val value = cell.getStringCellValue
                    if (value.nonEmpty) {
                        var process = startsWith.isEmpty
                        if (!process) {
                            startsWith.get.foreach(str => if (value.toUpperCase.startsWith(str.toUpperCase)) process = true)
                        }
                        if (process) {
                            regex.findFirstIn(value).map(x => res += x)
                        }
                    }
                }
            }
        }
        res
    }

    def readDataFromSheet(sheetName: String, colName: String,
                          numberValuesAsFlt: Boolean = true,
                          rowId: Int = 0, findNextRow: Boolean = false,
                          maxCellsNumber: Int = -1,
                          parseAlwaysAsString: Boolean = false,
                          rowCount: Int = -1,
                          startsWithColName: Boolean = false,
                          showMessage: Boolean = true
                         ): Seq[String] = {

        val sheet = workbookIn.getSheet(sheetName)
        if (null == sheet) throw new IllegalArgumentException("Error: workbook does not contain the sheet named as \"" + sheetName + "\"" +
            " file: " + excelFullFileName)

        var i = 0
        var founded = false
        var iRow = rowId
        val rowStart = sheet.getRow(iRow)
        val maxCellsIterate = if (-1 == maxCellsNumber) rowStart.getPhysicalNumberOfCells else maxCellsNumber
        while (i < maxCellsIterate && !founded) {
            val cell = rowStart.getCell(i)
            if (null == cell) {
                i = maxCellsIterate
            } else {
                if (Cell.CELL_TYPE_STRING == cell.getCellType &&
                    (cell.getStringCellValue.equals(colName) || (startsWithColName && cell.getStringCellValue.startsWith(colName)))
                ) {
                    founded = true
                } else {
                    i += 1
                }
            }
        }

        if (!founded && findNextRow) {
            val maxRowToSeek = if (-1 == rowCount) sheet.getPhysicalNumberOfRows - iRow else rowCount
            var iiRow = iRow + 1
            while (!founded && iiRow < maxRowToSeek) {
                val rowStart = sheet.getRow(iiRow)
                i = 0
                val maxCellsIterate = if (-1 == maxCellsNumber) rowStart.getPhysicalNumberOfCells else maxCellsNumber
                while (i < maxCellsIterate && !founded) {
                    val cell = rowStart.getCell(i)
                    if (null == cell) {
                        i = maxCellsIterate
                    } else {
                        if (Cell.CELL_TYPE_STRING == cell.getCellType &&
                            (cell.getStringCellValue.equals(colName) || (startsWithColName && cell.getStringCellValue.startsWith(colName)))
                        ) {
                            founded = true
                            iRow = iiRow
                        } else {
                            i += 1
                        }
                    }
                }
                iiRow += 1
            }
        }

        if (founded) {

            val cellType = if (!parseAlwaysAsString) {
                if (-1 == rowCount) {
                    try {
                        sheet.getRow(iRow + 1).getCell(i).getCellType
                    }
                    catch {
                        case npe: NullPointerException => throw new InternalError("Error: the first cell in column after column name cell contains \"null\" value")
                        case e: Exception =>
                            e.printStackTrace()
                            throw new UnknownError("Error: unknown error (see stack trace for more information)")
                    }
                } else {
                    var j = 1
                    while (null == sheet.getRow(iRow + j).getCell(i) && rowCount + 1 != j) j += 1
                    if (rowCount + 1 == j) throw new InternalError("Error: all " + rowCount + " cells in the column after the first contain \"null\" values")
                    sheet.getRow(iRow + j).getCell(i).getCellType
                }
            } else Cell.CELL_TYPE_STRING

            if (Cell.CELL_TYPE_ERROR == cellType)
                throw new InternalError("Error: the first notional cell in the column after column name cell contains \"error\" value")
            if (Cell.CELL_TYPE_BLANK == cellType)
                return null

            founded = false
            var ii = 0
            if (-1 == rowCount) {
                while (null != sheet.getRow(iRow + ii + 1) && null != sheet.getRow(iRow + ii + 1).getCell(i) &&
                    cellType == sheet.getRow(iRow + ii + 1).getCell(i).getCellType && !founded) {
                    cellType match {
                        case Cell.CELL_TYPE_STRING => if (sheet.getRow(iRow + ii + 1).getCell(i).getStringCellValue.isEmpty) founded = true
                        case _ =>
                    }
                    ii += 1
                }
            } else ii = rowCount

            val res = new Array[String](ii)
            for (idx <- 0 until ii) {
                res(idx) =
                    if (-1 != rowCount && null == sheet.getRow(iRow + idx + 1).getCell(i))
                        null
                    else
                        cellType match {
                            case Cell.CELL_TYPE_STRING =>
                                if (parseAlwaysAsString) {
                                    var res = try {
                                        sheet.getRow(iRow + idx + 1).getCell(i).getStringCellValue
                                    } catch {
                                        case e: Exception => ""
                                        case _ => ""
                                    }
                                    if (null == res) res = ""
                                    res
                                } else {
                                    sheet.getRow(iRow + idx + 1).getCell(i).getStringCellValue
                                }
                            case Cell.CELL_TYPE_NUMERIC => if (numberValuesAsFlt) sheet.getRow(iRow + idx + 1).getCell(i).getNumericCellValue.toString else sheet.getRow(iRow + idx + 1).getCell(i).getNumericCellValue.toInt.toString
                            case Cell.CELL_TYPE_FORMULA => sheet.getRow(iRow + idx + 1).getCell(i).getCellFormula
                            case Cell.CELL_TYPE_BOOLEAN => sheet.getRow(iRow + idx + 1).getCell(i).getBooleanCellValue.toString
                        }
            }

            indexOfStartRow = Option(iRow)
            countOfNonEmptyRows = Option(ii)

            res

        } else {

            //throw new IllegalArgumentException("Error: workbook does not contain column named as \"" + colName + "\" at row #" + iRow)
            if (showMessage) println(excelFullFileName + ": workbook does not contain column named as \"" + colName + "\" at row #\" + iRow")
            new Array[String](0)
        }

    }

    def writeTable(
                      sheetName: String,
                      oxTableLabels: Seq[String],
                      oyTableLabels: Seq[String],
                      data: Seq[Seq[String]]
                  ): Unit = {

        val sheet = workbookOut.createSheet(sheetName)

        var iRow = 0

        if (oxTableLabels.nonEmpty) {
            val rawHeader = sheet.createRow(iRow)
            var i = 0
            if (oyTableLabels.nonEmpty) {
                rawHeader.createCell(i)
                i += 1
            }
            oxTableLabels.foreach { label =>
                rawHeader.createCell(i).setCellValue(label)
                i += 1
            }
            iRow += 1
        }


        data.foreach { oyDataRow =>

            var iCol = 0
            val row = sheet.createRow(iRow)

            if (oyTableLabels.nonEmpty) {
                val idx = if (oxTableLabels.nonEmpty) iRow - 1 else iRow
                row.createCell(iCol).setCellValue(oyTableLabels(idx))
                iCol += 1
            }

            oyDataRow.foreach { data =>
                row.createCell(iCol).setCellValue(data)
                iCol += 1
            }
            iRow += 1
        }

        if (oxTableLabels.isEmpty && oyTableLabels.isEmpty && data.isEmpty) {
            sheet.createRow(0).createCell(0).setCellValue("0")
        }

        val fos = new FileOutputStream(new File(excelFullFileName))
        workbookOut.write(fos)
        fos.close()
    }
}