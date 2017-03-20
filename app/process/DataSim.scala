package process

import java.util.{Random, UUID}


object DataSim {

    val PossibleTrainsLiterals: Array[String] = Array[String](
        "A", "Б", "В", "Г", "Д", "Е", "Ж", "И", "К", "Л", "М", "Н", "О",
        "П", "Р", "С", "Ц", "У", "Ч", "Ш", "Э", "Я")

    val PossibleTrainsRanges: Array[(Int, Int)] = Array[(Int, Int)](
        1 -> 150, 301 -> 450, 701 -> 750
    )

}

class DataSim(allData: Seq[FullInfo]) {

    def simulateUUIDs() : Unit = {
        allData.map{ fullInfo =>
            fullInfo.uuid = UUID.randomUUID.toString
            fullInfo
        }
    }

    def simulateResponsibles(
                                responsiblesSeed: Seq[String]
                            ): Unit = {

        val rndObj = new Random
        allData.map { fullInfo =>
            if ("" == fullInfo.responsible) {
                val idx = rndObj.nextInt(responsiblesSeed.length)
                fullInfo.responsible = responsiblesSeed(idx)
            }
            fullInfo
        }
    }

    def simulateResolvingStatuses(
                                     percentileUnresolved: Float,
                                     percentileInProgress: Float
                                 ): Unit = {

        assert(percentileInProgress >= 0 && percentileUnresolved >= 0 && percentileUnresolved + percentileInProgress <= 1.0F,
            "Error to simulate resolving statuses: wrong percentile values"
        )

        val rndObj = new Random
        allData.map { fullInfo =>
            if (ResolvingStatus.Unknown == fullInfo.resolvingStatus) {
                val rndFlt = rndObj.nextFloat
                if (rndFlt <= percentileUnresolved) fullInfo.resolvingStatus = ResolvingStatus.Unresolved
                else if (rndFlt <= percentileInProgress + percentileUnresolved) fullInfo.resolvingStatus = ResolvingStatus.InProgress
                else fullInfo.resolvingStatus = ResolvingStatus.Resolved
            }
            fullInfo
        }

    }

    def simulateAffiliates(
                              keysAffiliateToReplace: Seq[String],
                              affiliatesSeed: Seq[String]
                          ): Unit = {

        val rndObj = new Random
        allData.map { fullInfo =>
            if (
                null == fullInfo.affiliate ||
                    "" == fullInfo.affiliate || {
                    var res = false
                    keysAffiliateToReplace.foreach(keyWord => res = res || fullInfo.affiliate.contains(keyWord))
                    res
                }
            ) {
                fullInfo.affiliate =
                    affiliatesSeed(rndObj.nextInt(affiliatesSeed.length))
            }
            fullInfo
        }
    }

    def simulateTrainsCodes(
                               numberOfTrains: Int,
                               affiliatesSeed: Seq[String]
                           ): Seq[String] = {

        assert(affiliatesSeed.nonEmpty,
            "Error to simulate train codes: affiliates seed is empty"
        )
        assert(affiliatesSeed.length < numberOfTrains,
            "Error to simulate train codes: number of affiliates must be less than number of trains"
        )

        var trainsCodes = new Array[String](numberOfTrains)
        val rndObj = new Random

        var maxRange = 0
        DataSim.PossibleTrainsRanges.foreach { range => maxRange += range._2 - range._1 + 1 }

        assert(maxRange > numberOfTrains,
            "Error to simulate train codes: number of trains is less than max range, max range value: " + maxRange
        )

        var i = 0
        while (i < numberOfTrains) {
            var repeatGenerateTrainCode = true
            while (repeatGenerateTrainCode) {
                var trainCode = rndObj.nextInt(maxRange)
                var ii = 0
                while (ii < DataSim.PossibleTrainsRanges.length - 1) {
                    if (trainCode > DataSim.PossibleTrainsRanges(ii)._2) {
                        trainCode -= DataSim.PossibleTrainsRanges(ii)._2
                        trainCode += DataSim.PossibleTrainsRanges(ii + 1)._1
                    }
                    ii += 1
                }
                val trainCodeStr = ("000" + trainCode.toString).substring(trainCode.toString.length)
                repeatGenerateTrainCode = trainsCodes.contains(trainCodeStr)
                if (!repeatGenerateTrainCode) trainsCodes(i) = trainCodeStr
            }
            i += 1
        }

        trainsCodes = trainsCodes.map { trainCodeStr =>
            trainCodeStr +
                DataSim.PossibleTrainsLiterals(rndObj.nextInt(DataSim.PossibleTrainsLiterals.length))
        }

        val iterationIndexesByAffiliates = Array.fill[Int](affiliatesSeed.length)(0)
        val rangesOfTrainsCodesByAffiliates = new Array[(Int, Int)](affiliatesSeed.length)
        val stepOfRange = numberOfTrains / affiliatesSeed.length
        rangesOfTrainsCodesByAffiliates(0) = 0 ->
            (if (affiliatesSeed.nonEmpty) stepOfRange else numberOfTrains - 1)
        i = 1
        while (i < affiliatesSeed.length) {
            val first = rangesOfTrainsCodesByAffiliates(i - 1)._2 + 1
            rangesOfTrainsCodesByAffiliates(i) =
                first -> (first + stepOfRange)
            if (affiliatesSeed.length - 1 == i)
                rangesOfTrainsCodesByAffiliates(i) = rangesOfTrainsCodesByAffiliates(i)._1 -> (numberOfTrains - 1)
            i += 1
        }
        i = 0
        while (i < allData.length) {
            val fullInfo = allData(i)
            if ("" == fullInfo.trainCode) {
                val affiliateIdx = affiliatesSeed.indexOf(fullInfo.affiliate)
                val ii = iterationIndexesByAffiliates(affiliateIdx)
                fullInfo.trainCode = trainsCodes(ii)
                val range = rangesOfTrainsCodesByAffiliates(affiliateIdx)
                if (ii == range._2) {
                    iterationIndexesByAffiliates(affiliateIdx) = range._1
                } else {
                    iterationIndexesByAffiliates(affiliateIdx) = ii + 1
                }
            }

            i += 1
        }

        trainsCodes
    }

    def simulateTrainStaffsCodes(
                                    numberOfStaffsRange: (Int, Int),
                                    trainsCodesSeed: Seq[String]
                                ): Seq[Seq[String]] = {

        assert(numberOfStaffsRange._1 > -1 && numberOfStaffsRange._1 <= numberOfStaffsRange._2,
            "Error to simulate train staffs codes: incorrect number of staffs range"
        )

        val trainStaffsCodes = new Array[Seq[String]](trainsCodesSeed.length)
        val rndObj = new Random
        trainStaffsCodes.indices.foreach { idx =>
            val numberOfStaffs = rndObj.nextInt(numberOfStaffsRange._2 - numberOfStaffsRange._1) +
                numberOfStaffsRange._1
            val innerArr = new Array[String](numberOfStaffs)
            var i = 0
            while (i < numberOfStaffs) {
                val firstDigit = rndObj.nextInt(10)
                val lastDigit = rndObj.nextInt(10)
                innerArr(i) = firstDigit.toString + lastDigit.toString
                i += 1
            }
            trainStaffsCodes(idx) = innerArr
        }

        val iterationIndexesByTrainsCodes = Array.fill[Int](trainStaffsCodes.length)(0)
        allData.map { fullInfo =>
            if ("" == fullInfo.staffCode) {
                val trainCodeIdx = trainsCodesSeed.indexOf(fullInfo.trainCode)
                if (-1 != trainCodeIdx) {
                    val i = iterationIndexesByTrainsCodes(trainCodeIdx)
                    fullInfo.staffCode = trainStaffsCodes(trainCodeIdx)(i)
                    if (trainStaffsCodes(trainCodeIdx).length - 1 == i) {
                        iterationIndexesByTrainsCodes(trainCodeIdx) = 0
                    } else {
                        iterationIndexesByTrainsCodes(trainCodeIdx) = i + 1
                    }
                }
            }

            fullInfo
        }

        trainStaffsCodes
    }

    def simulateProgressDates(): Unit = {
        
    }
}
