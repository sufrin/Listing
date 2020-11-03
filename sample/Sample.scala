/**
  *
  * A simple source-file listing program
  *
  */

import java.time.LocalDateTime

import com.itextpdf.io.font.PdfEncodings
import com.itextpdf.kernel.geom.PageSize
import com.itextpdf.kernel.pdf.canvas.PdfCanvas
import com.itextpdf.layout.property.{AreaBreakType, TextAlignment}
import com.itextpdf.kernel.events.{Event, IEventHandler, PdfDocumentEvent}
import com.itextpdf.kernel.font.{PdfFont, PdfFontFactory}
import com.itextpdf.kernel.pdf._
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.{AreaBreak, Paragraph, Text}

import scala.io.Source
import Utilities._



object  Sample
{
  val INVERTEDPORTRAIT = new PdfNumber(180)
  val LANDSCAPE        = new PdfNumber(90)
  val PORTRAIT         = new PdfNumber(0)
  val SEASCAPE         = new PdfNumber(270)
  val courier: PdfFont = PdfFontFactory.createRegisteredFont("courier")
  val symbol:  PdfFont = PdfFontFactory.createRegisteredFont("symbol", PdfEncodings.SYMBOL)

  object Indent
  { private val NBSP  = "\u00a0" // Non-breaking space -- known to exist in Courier
  private val cache = Array.ofDim[Text](250)
    /** Yields a text of `n` non-breaking space characters in a font that is known to have an nbsp glyph. */
    def apply(n: Int): Text =
    {
      if (n<cache.length)
      {
        var r = cache(n)
        if (r == null)
        { r = new Text(NBSP * n).setFont(courier); cache(n) = r }
        r
      }
      else {
        // Preposterously large indentations don't get cached
        Console.err.println(s"Large line indentation $n (>${cache.length}) uncached")
        new Text(NBSP * n).setFont(courier)
      }
    }
  }

  def createPDF(dest: String, fontSize: Float, landscape: Boolean): Document =
  {
    var writer  = new PdfWriter(dest).setSmartMode(true)
    val pdf     = new PdfDocument(writer)
    val doc     = new Document(pdf, if (landscape) PageSize.A4.rotate else PageSize.A4) // Shape and orientation of the page
    val longEdge  = PageSize.A4.getHeight
    val shortEdge = PageSize.A4.getWidth

    // Console.err.println(if (landscape) s"$longEdge x $shortEdge" else s"$shortEdge x $longEdge")

    doc.setTextAlignment(TextAlignment.LEFT)
    .setFont(courier)
    .setFontSize(fontSize)

    doc.setTopMargin(30)
    doc.setBottomMargin(30)
    doc.setLeftMargin(30)
    doc.setRightMargin(30)

    //
    // Viewer preferences
    //
    if (true)
    {
      val preferences = new PdfViewerPreferences
      preferences.setFitWindow(true)
      preferences.setHideMenubar(false)
      preferences.setHideToolbar(true)
      preferences.setHideWindowUI(false)
      preferences.setCenterWindow(false)
      preferences.setDisplayDocTitle(false)
      preferences.setDuplex(PdfViewerPreferences.PdfViewerPreferencesConstants.DUPLEX_FLIP_LONG_EDGE)
      pdf.getCatalog.setViewerPreferences(preferences)
    }

    doc
  }



  val usage : String =
    """""".stripMargin

  val SAMPLE: String =
    """forall \u2200 exists \u2203 A \u00c4""".stripMargin

  def main(args: Array[String]): Unit =
  {
    val sources     = new scala.collection.mutable.Queue[String]
    var fontSize    = 12.0f
    var headSize    = 10.0f
    var orientation = PORTRAIT
    var startOnOdd  = false
    var opt         = true

    for (arg <- args)
      if (opt && arg.matches("-s=[0-9]+([.0-9]*)?")) fontSize = arg.substring(3).toFloat else
      if (opt && arg.matches("-S=[0-9]+([.0-9]*)?")) headSize = arg.substring(3).toFloat else
      if (opt && arg.matches("-[pP]")) orientation = PORTRAIT else
      if (opt && arg.matches("-[cC]")) startOnOdd = false else
      if (opt && arg.matches("--")) opt = false else
      if (opt && arg.matches("-.*")) { Console.println(usage); return } else
      { opt = false
        sources.enqueue(arg)
      }


    val doc       = createPDF("SAMPLE.pdf", fontSize, orientation==LANDSCAPE)
    val startPage = new PageStartHandler(doc, orientation, "", "", courier, headSize, startOnOdd)
    doc.getPdfDocument.addEventHandler(PdfDocumentEvent.START_PAGE, startPage)

    def makeSample(para: Paragraph, fontPath: String, fontSize: Float): Unit =
    { 
      val font =
      try
        {
          if (fontPath.endsWith(".ttc")) PdfFontFactory.createTtcFont(fontPath, 0, PdfEncodings.IDENTITY_H, true, true)
          else
          if (fontPath.endsWith(".ttf")) PdfFontFactory.createFont(fontPath, PdfEncodings.IDENTITY_H, true, true)
          else
          if (fontPath=="symbol") symbol
          else
            PdfFontFactory.createRegisteredFont(fontPath)
        }
      catch { case exn: Exception => Console.err.println(exn); return }

      def putGlyph(code: Int): Unit =
      {
        if (try { font.containsGlyph(code) } catch { case exn : Throwable => Console.err.println(f"$code%04x $exn%s"); false})
        {
          para.add(new Text(f" $code%04x=").setFont(courier).setFontSize(fontSize))
          try
          {
            para.add(new Text(f"${code.toChar}%c").setFont(font).setFontSize(fontSize))
          }
          catch
            {
              case exn: NullPointerException => para.add("(**)")
            }
        }
      }

      for (code <- 32 to 127) putGlyph(code)
      para.add("\n")
      for (code <- 128 to 255) putGlyph(code)
      para.add("\n")
      for (code <- 0x2190 to 0x22ff) putGlyph(code)

    }

    var files   = 0
    for (source<-sources)
    {
      Console.err.println(source)
      val para = new Paragraph().setFont(courier).setFontSize(fontSize)
      startPage.leftHead   = source
      startPage.pageNumber = 0
      // break pages between files
      if (files>0) doc.add(new AreaBreak(AreaBreakType.NEXT_PAGE))
      files += 1
      //
      makeSample(para, source, fontSize)
      doc.add(para)
    }
    doc.add(new AreaBreak(AreaBreakType.LAST_PAGE))
    doc.close()
  }
}
