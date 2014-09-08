package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import net.liftmodules.JQueryModule
import net.liftweb.http.js.jquery._

import code.comet._


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("code")

    // Build SiteMap
    val entries = List(
      Menu.i("Home") / "index",
      Menu.i("Draft Lobby") / "draftLobby",
      Menu.i("Winston Draft") / "winston-draft" >> Hidden,
      Menu.i("Draft") / "draft" >> Hidden
    )

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMap(SiteMap(entries:_*))

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    
    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    //Init the jQuery module, see http://liftweb.net/jquery for more information.
    LiftRules.jsArtifacts = JQueryArtifacts
    JQueryModule.InitParam.JQuery=JQueryModule.JQuery191
    JQueryModule.init()

    LiftRules.dispatch.append {
      case Req("download" :: _, _, GetRequest) =>
        () => {
          val data = DeckList.is.openOr("Empty").getBytes
          val fname = FileName.is.openOr("winston-draft.dec")
          
          val headers = List(
            ("Content-type" -> "text/plain"), 
            ("Content-length" -> data.length.toString), 
            ("Content-disposition" -> s"attachment; filename=$fname"))
            
    
          Full(StreamingResponse(
            new java.io.ByteArrayInputStream(data),
            () => {},
           data.length, 
            headers, Nil, 200))
        }
    }

  }

  
}
