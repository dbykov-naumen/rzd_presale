package controllers

import java.util.Random
import javax.inject.{Inject, Singleton}

import play.api.libs.json.{JsNumber, JsObject, _}
import play.api.mvc.{Action, Controller}
import process.FullInfo
import represent.DataPresentServiceExcel
import utils.CORSHelper

case class RatingStatusesCounted(
                                    countHardNegative: Int,
                                    countSoftNegative: Int,
                                    countNeutral: Int,
                                    countPositive: Int
                                )

case class ResolvingStatusesCounted(
                                       countResolved: Int,
                                       countUnresolved: Int,
                                       countInProgress: Int
                                   )

object DataController {

    val MulTrainsNeutral = 1.45F
    val MaxTrainsNeutral = 0.9F
    val MulStaffsNeutral = 1.15F
    val MaxStaffsNeutral = 0.8F
}

@Singleton
class DataController @Inject()(configuration: play.api.Configuration)
    extends Controller
        with CORSHelper {

    private def representFullInfoAsJsonObject(fullInfo: FullInfo): JsObject = {

        JsObject(Array[(String, JsValue)](
            FullInfo.CaseArgumentsAsString.uuid -> JsString(fullInfo.uuid),
            FullInfo.CaseArgumentsAsString.date -> JsString(fullInfo.date),
            FullInfo.CaseArgumentsAsString.source -> JsString(fullInfo.source),
            FullInfo.CaseArgumentsAsString.cipher -> JsString(fullInfo.cipher),
            FullInfo.CaseArgumentsAsString.call -> JsString(fullInfo.call),
            FullInfo.CaseArgumentsAsString.subdivision -> JsString(fullInfo.subdivision),
            FullInfo.CaseArgumentsAsString.addressed -> JsString(fullInfo.addressed),
            FullInfo.CaseArgumentsAsString.affiliate -> JsString(fullInfo.affiliate),
            FullInfo.CaseArgumentsAsString.responsible -> JsString(fullInfo.responsible),
            FullInfo.CaseArgumentsAsString.comment -> JsString(fullInfo.comment),
            FullInfo.CaseArgumentsAsString.category -> JsString(fullInfo.category),
            FullInfo.CaseArgumentsAsString.trainCode -> JsString(fullInfo.trainCode),
            FullInfo.CaseArgumentsAsString.staffCode -> JsString(fullInfo.staffCode),
            FullInfo.CaseArgumentsAsString.ratingStatus.toString -> JsString(fullInfo.ratingStatus.toString),
            FullInfo.CaseArgumentsAsString.resolvingStatus.toString -> JsString(fullInfo.resolvingStatus.toString)
        ))
    }

    private def simulateRatingStatusesCounted(
                                                 initCounted: RatingStatusesCounted,
                                                 mul: Float,
                                                 max: Float
                                             ): RatingStatusesCounted = {

        val totalNonNeutralStatuses = initCounted.countHardNegative + initCounted.countSoftNegative + initCounted.countPositive
        val totalRatingStatuses = totalNonNeutralStatuses + initCounted.countNeutral
        var resTotalNeutral = (initCounted.countNeutral.toFloat * mul).toInt
        if (resTotalNeutral > (max * totalRatingStatuses.toFloat).toInt)
            resTotalNeutral = ((max + new Random().nextInt(2).toFloat / 100.0F) * totalRatingStatuses.toFloat).toInt
        val resTotalPositive =
            ((initCounted.countPositive.toFloat / totalNonNeutralStatuses.toFloat) *
                (totalRatingStatuses - resTotalNeutral).toFloat).toInt
        val resTotalHardNegative =
            ((totalRatingStatuses - resTotalNeutral - resTotalPositive).toFloat *
                (initCounted.countHardNegative.toFloat / (initCounted.countHardNegative + initCounted.countSoftNegative).toFloat)
                ).toInt
        var resTotalSoftNegative =
            ((totalRatingStatuses - resTotalNeutral - resTotalPositive).toFloat *
                (initCounted.countSoftNegative.toFloat / (initCounted.countHardNegative + initCounted.countSoftNegative).toFloat)
                ).toInt
        if (resTotalNeutral + resTotalHardNegative + resTotalSoftNegative + resTotalPositive < totalRatingStatuses) {
            resTotalSoftNegative += totalRatingStatuses - resTotalNeutral - resTotalHardNegative - resTotalSoftNegative - resTotalPositive
        }

        RatingStatusesCounted(
            countHardNegative = resTotalHardNegative,
            countSoftNegative = resTotalSoftNegative,
            countNeutral = resTotalNeutral,
            countPositive = resTotalPositive)
    }

    def getItems(
                    moods: Option[String],
                    regionId: Option[String],
                    draw: Int = 1
                ) = Action { implicit request =>

        val presentServiceExcel = new DataPresentServiceExcel(
            configuration.underlying.getString("excelFullFileName")
        )
        val allCalls = presentServiceExcel.getCalls(
            uuid = None,
            ratingStatus = moods,
            affiliate = regionId
        )

        if (allCalls.nonEmpty) {

            val calls = if (allCalls.length > draw * 10) allCalls.splitAt(draw * 10)._1.splitAt((draw - 1) * 10)._2
            else allCalls

            val jsonRes = JsObject(Array[(String, JsValue)](
                "draw" -> JsNumber(draw),
                "start" -> JsNumber(draw * 10),
                "length" -> JsNumber(10),
                "recordsTotal" -> JsNumber(allCalls.length),
                "recordsFiltered" -> JsNumber(allCalls.length),
                "items" -> JsArray(
                    calls.map {
                        fullInfo =>
                            representFullInfoAsJsonObject(fullInfo)
                    }
                )
            ))

            Ok(Json.prettyPrint(jsonRes)).
                as("application/json").
                allowForAllDomains

        } else {
            Ok("{\"items\":[], \"recordsTotal\": 0, \"recordsFiltered\": 0}").as("application/json").allowForAllDomains
        }
    }

    def getItem(uuid: String) = Action { implicit request =>

        val presentServiceExcel = new DataPresentServiceExcel(
            configuration.underlying.getString("excelFullFileName")
        )
        val calls = presentServiceExcel.getCalls(Option(uuid))

        if (calls.nonEmpty)

            Ok(Json.prettyPrint(representFullInfoAsJsonObject(calls.head))).
                as("application/json").
                allowForAllDomains

        else
            Ok.as("application/json").allowForAllDomains
    }

    def getInfo(
                   regionId: Option[String]
               ) = Action { implicit request =>


        val presentServiceExcel = new DataPresentServiceExcel(
            configuration.underlying.getString("excelFullFileName")
        )

        val nowTime = System.currentTimeMillis

        val ratingStatusesByMonths = presentServiceExcel.ratingStatusesByMonths(
            regionId
        )
        val totalRatingStatusesCounted = RatingStatusesCounted(
            countHardNegative = ratingStatusesByMonths.map(x => x.totalHardNegative).sum,
            countSoftNegative = ratingStatusesByMonths.map(x => x.totalSoftNegative).sum,
            countNeutral = ratingStatusesByMonths.map(x => x.totalNeutral).sum,
            countPositive = ratingStatusesByMonths.map(x => x.totalPositive).sum
        )
        val trainsRatingStatusesCounted = simulateRatingStatusesCounted(
            totalRatingStatusesCounted, DataController.MulTrainsNeutral, DataController.MaxTrainsNeutral
        )
        val staffsRatingStatusesCounted = simulateRatingStatusesCounted(
            totalRatingStatusesCounted, DataController.MulStaffsNeutral, DataController.MaxStaffsNeutral
        )
        val ratingStatusesByAffiliates = presentServiceExcel.ratingStatusesByAffiliates()

        val resolvingStatusesByMonths = presentServiceExcel.resolvingStatusesByMonths(
            regionId
        )
        val totalResolvingStatusesCounted = ResolvingStatusesCounted(
            countResolved = resolvingStatusesByMonths.map(x => x.totalResolved).sum,
            countUnresolved = resolvingStatusesByMonths.map(x => x.totalUnresolved).sum,
            countInProgress = resolvingStatusesByMonths.map(x => x.totalInProgress).sum
        )

        val resolvingStatusesByResponsibles = presentServiceExcel.resolvingStatusesByResponsibles(
            regionId
        ).filter(x => null != x.responsible && "" != x.responsible)
        val resolvingStatusesByCategories = presentServiceExcel.resolvingStatusesByCategories(
            regionId
        )
        val resolvingStatusesBySources = presentServiceExcel.resolvingStatusesBySources(
            regionId
        )
        val resolvingStatusesByAffiliates = presentServiceExcel.resolvingStatusesByAffiliates()


        val top3OldestCalls = presentServiceExcel.timelineNonresolvedCalls(
            numberOfTop = 3,
            oldestNotNewest = true,
            affiliate = regionId
        )

        val top3BestResponsibles = presentServiceExcel.responsiblesByResolvedLastMonth(
            numberOfTop = 3,
            bestsNotWorsts = true,
            resolvingRateRangeMinutes = 20 -> 40,
            affiliate = regionId
        )
        val top3WorstResponsibles = presentServiceExcel.responsiblesByResolvedLastMonth(
            numberOfTop = 3,
            bestsNotWorsts = false,
            resolvingRateRangeMinutes = 35 -> 60,
            affiliate = regionId
        )

        val jsonRes = JsObject(Array[(String, JsValue)](
            "now" -> JsNumber(nowTime),
            "emotes" -> JsObject(Array[(String, JsValue)](
                "timeline" -> JsArray(
                    ratingStatusesByMonths.map { x =>
                        JsObject(Array[(String, JsValue)](
                            "month" -> JsNumber(x.month),
                            "year" -> JsNumber(2016),
                            "positive" -> JsNumber(x.totalPositive),
                            "hardNegative" -> JsNumber(x.totalHardNegative),
                            "softNegative" -> JsNumber(x.totalSoftNegative),
                            "neutral" -> JsNumber(x.totalNeutral)
                        ))
                    }
                ),
                "trains" -> JsObject(Array[(String, JsValue)](
                    "positive" -> JsNumber(trainsRatingStatusesCounted.countPositive),
                    "hardNegative" -> JsNumber(trainsRatingStatusesCounted.countHardNegative),
                    "softNegative" -> JsNumber(trainsRatingStatusesCounted.countSoftNegative),
                    "neutral" -> JsNumber(trainsRatingStatusesCounted.countNeutral)
                )),
                "teams" -> JsObject(Array[(String, JsValue)](
                    "positive" -> JsNumber(staffsRatingStatusesCounted.countPositive),
                    "hardNegative" -> JsNumber(staffsRatingStatusesCounted.countHardNegative),
                    "softNegative" -> JsNumber(staffsRatingStatusesCounted.countSoftNegative),
                    "neutral" -> JsNumber(staffsRatingStatusesCounted.countNeutral)
                )),
                "regions" -> JsArray(ratingStatusesByAffiliates.map { x =>
                    JsObject(Array[(String, JsValue)](
                        "title" -> JsString(x.affiliate),
                        "regionId" -> JsString(x.affiliate),
                        "counts" -> JsObject(Array[(String, JsValue)](
                            "total" -> JsNumber(x.totalHardNegative + x.totalSoftNegative
                                + x.totalPositive + x.totalNeutral),
                            "positive" -> JsNumber(x.totalPositive),
                            "hardNegative" -> JsNumber(x.totalHardNegative),
                            "softNegative" -> JsNumber(x.totalSoftNegative),
                            "neutral" -> JsNumber(x.totalNeutral)
                        ))
                    ))
                })
            )),
            "statistic" -> JsObject(Array[(String, JsValue)](
                "all" -> JsObject(Array[(String, JsValue)](
                    "total" -> JsNumber(totalResolvingStatusesCounted.countInProgress +
                        totalResolvingStatusesCounted.countResolved +
                        totalResolvingStatusesCounted.countUnresolved),
                    "resolved" -> JsNumber(totalResolvingStatusesCounted.countResolved),
                    "inProgress" -> JsNumber(totalResolvingStatusesCounted.countInProgress),
                    "unresolved" -> JsNumber(totalResolvingStatusesCounted.countUnresolved)
                )),
                "byResponsible" -> JsArray(resolvingStatusesByResponsibles.map { x =>
                    JsObject(Array[(String, JsValue)](
                        "title" -> JsString(x.responsible),
                        "regionId" -> JsString(regionId.getOrElse("")),
                        "counts" -> JsObject(Array[(String, JsValue)](
                            "total" -> JsNumber(x.totalInProgress + x.totalResolved + x.totalUnresolved),
                            "resolved" -> JsNumber(x.totalResolved),
                            "inProgress" -> JsNumber(x.totalInProgress),
                            "unresolved" -> JsNumber(x.totalUnresolved)
                        ))
                    ))
                }),
                "byCategory" -> JsArray(resolvingStatusesByCategories.map { x =>
                    JsObject(Array[(String, JsValue)](
                        "title" -> JsString(x.category),
                        "regionId" -> JsString(regionId.getOrElse("")),
                        "counts" -> JsObject(Array[(String, JsValue)](
                            "total" -> JsNumber(x.totalInProgress + x.totalResolved + x.totalUnresolved),
                            "resolved" -> JsNumber(x.totalResolved),
                            "inProgress" -> JsNumber(x.totalInProgress),
                            "unresolved" -> JsNumber(x.totalUnresolved)
                        ))
                    ))
                }),
                "bySource" -> JsArray(resolvingStatusesBySources.map { x =>
                    JsObject(Array[(String, JsValue)](
                        "title" -> JsString(x.source),
                        "regionId" -> JsString(regionId.getOrElse("")),
                        "counts" -> JsObject(Array[(String, JsValue)](
                            "total" -> JsNumber(x.totalInProgress + x.totalResolved + x.totalUnresolved),
                            "resolved" -> JsNumber(x.totalResolved),
                            "inProgress" -> JsNumber(x.totalInProgress),
                            "unresolved" -> JsNumber(x.totalUnresolved)
                        ))
                    ))
                })
            )),
            "regions" -> JsArray(resolvingStatusesByAffiliates.map { x =>
                JsObject(Array[(String, JsValue)](
                    "title" -> JsString(x.affiliate),
                    "regionId" -> JsString(x.affiliate),
                    "counts" -> JsObject(Array[(String, JsValue)](
                        "total" -> JsNumber(x.totalInProgress + x.totalResolved + x.totalUnresolved),
                        "resolved" -> JsNumber(x.totalResolved),
                        "inProgress" -> JsNumber(x.totalInProgress),
                        "unresolved" -> JsNumber(x.totalUnresolved)
                    ))
                ))
            }),
            "oldest" -> JsArray(top3OldestCalls.map { x =>
                representFullInfoAsJsonObject(x)
            }),
            "bests" -> JsArray(top3BestResponsibles.map { x =>
                JsObject(Array[(String, JsValue)](
                    "regionId" -> JsString(x.affiliate),
                    "title" -> JsString(x.responsible),
                    "processed" -> JsNumber(x.numberResolved),
                    "avgMinutes" -> JsNumber(x.avgTimeResolvingMinutes)
                ))
            }),
            "worst" -> JsArray(top3WorstResponsibles.map { x =>
                JsObject(Array[(String, JsValue)](
                    "regionId" -> JsString(x.affiliate),
                    "title" -> JsString(x.responsible),
                    "processed" -> JsNumber(x.numberResolved),
                    "avgMinutes" -> JsNumber(x.avgTimeResolvingMinutes)
                ))
            })
        ))

        Ok(Json.prettyPrint(jsonRes)).
            as("application/json").
            allowForAllDomains
    }
}
