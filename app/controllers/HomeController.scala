package controllers

import javax.inject.{Inject, Singleton}

import play.api.mvc.{Action, Controller}

@Singleton
class HomeController @Inject() extends Controller {

    def index() = Action { implicit request =>

        Ok(views.html.index())
    }
}
