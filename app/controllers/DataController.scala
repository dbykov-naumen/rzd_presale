package controllers

import javax.inject.{Inject, Singleton}

import play.api.libs.json.{JsObject, _}
import play.api.mvc.{Action, Controller}
import process.FullInfo
import represent.DataPresentServiceExcel
import utils.CORSHelper

@Singleton
class DataController @Inject() (configuration: play.api.Configuration)
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

            val  calls = if (allCalls.length > draw * 10) allCalls.splitAt(draw * 10)._1.splitAt((draw - 1) * 10)._2
            else allCalls

            val jsonRes = JsObject(Array[(String, JsValue)](
                "draw" -> JsNumber(draw),
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
            BadRequest
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
            BadRequest
    }

    def getInfo(
                   regionId: Option[String]
               ) = Action { implicit request =>


        val nowTime = System.currentTimeMillis
        Ok
    }
}
