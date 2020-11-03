/**
  *
  * A simple source-file listing program
  *
  */

import java.time.LocalDateTime

import Utilities._
import com.itextpdf.io.font.PdfEncodings
import com.itextpdf.kernel.events.PdfDocumentEvent
import com.itextpdf.kernel.font.{PdfFont, PdfFontFactory}
import com.itextpdf.kernel.geom.PageSize
import com.itextpdf.kernel.pdf._
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.{AreaBreak, Paragraph, Text}
import com.itextpdf.layout.property.{AreaBreakType, TextAlignment}

import scala.io.Source




object  Listing
{
  val INVERTEDPORTRAIT = new PdfNumber(180)
  val LANDSCAPE        = new PdfNumber(90)
  val PORTRAIT         = new PdfNumber(0)
  val SEASCAPE         = new PdfNumber(270)
  var courier          = true
  var fontFile         = ""
  val headFont: PdfFont = PdfFontFactory.createRegisteredFont("helvetica")
  lazy val bodyFont: PdfFont =
    if (courier) PdfFontFactory.createRegisteredFont("courier")
      else
      try {
        PdfFontFactory.createFont(fontFile, PdfEncodings.IDENTITY_H, true, true)
      }
      catch { case exn: Exception => Console.err.println(exn); headFont }


  object Indent
  { private val NBSP  = "\u00a0" // Non-breaking space -- known to exist in Courier
    private val cache = Array.ofDim[Text](250)
    private val indentFont =
      if (bodyFont.containsGlyph('\u00a0')) bodyFont else headFont
    /** Yields a text of `n` non-breaking space characters in a font that is known to have an nbsp glyph. */
    def apply(n: Int): Text =
    {
      if (n<cache.length)
      {
        var r = cache(n)
        if (r == null)
        { r = new Text(NBSP * n).setFont(indentFont); cache(n) = r }
        r
      }
      else {
        // Preposterously large indentations don't get cached
        Console.err.println(s"Large line indentation $n (>${cache.length}) uncached")
        new Text(NBSP * n).setFont(headFont)
      }
    }
  }

  def createPDF(dest: String, fontSize: Float, landscape: Boolean): Document =
  {
    val writer  = new PdfWriter(dest).setSmartMode(true)
    val pdf     = new PdfDocument(writer)
    val doc     = new Document(pdf, if (landscape) PageSize.A4.rotate else PageSize.A4) // Shape and orientation of the page
    //val longEdge  = PageSize.A4.getHeight
    //val shortEdge = PageSize.A4.getWidth

    // Console.err.println(if (landscape) s"$longEdge x $shortEdge" else s"$shortEdge x $longEdge")

    doc.setTextAlignment(TextAlignment.LEFT)
    .setFont(bodyFont)
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
    """Listing usage: [options] filepath*
      | options are:
      | -s=<float>  text font size (8.0)
      | -S=<float>  header font size (10.0)
      | -p          portrait orientation (default is landscape)
      | -c          compress, ie. don't start new sheet per file
      | -f=<font>   use <font> as text font
      | -enc=<enc>  set input encoding (default UTF8)
      | <path>.pdf  (once only) set output pdf file path (default is LISTING.pdf)
      | -o          allow output pdf file path to be set again
      | --          remaining arguments are filepaths""".stripMargin


  def main(args: Array[String]): Unit =
  {
    val sources     = new scala.collection.mutable.Queue[String]
    var fontSize    = 8.0f
    var headSize    = 10.0f
    var orientation = LANDSCAPE
    var startOnOdd  = true
    var opt         = true
    var inputEnc    = "UTF8"
    var out         = false
    var outFile     = "LISTING.pdf"

    for (arg <- args) {
      if (opt && arg.matches("-s=[0-9]+([.0-9]*)?")) fontSize = arg.substring(3).toFloat else
      if (opt && arg.matches("-S=[0-9]+([.0-9]*)?")) headSize = arg.substring(3).toFloat else
      if (opt && arg.matches("-f=.+")) { courier=false; fontFile = arg.substring(3) } else
      if (opt && arg.matches("-[pP]")) orientation = PORTRAIT else
      if (opt && arg.matches("-[cC]")) startOnOdd = false else
      if (opt && arg.matches("-[oO]")) out=false else
      if (opt && arg.matches("--")) opt = false else
      if (opt && arg.matches("-enc=.+")) inputEnc=arg.substring(5) else
      if (opt && arg.matches("-.*")) { Console.err.println(usage); return } else
      { if (arg.endsWith(".pdf") || arg.endsWith(".PDF")) {
          if (out) {
            Console.err.println("Warning: output pdf file path can be set only once (" + arg +")")
          } else {outFile = arg; out=true }
        } else
        {
          opt = false
          if (new java.io.File(arg).exists)
             sources.enqueue(arg)
          else
             Console.err.println(s"File not found: $arg")
        }
      }
    }


    val doc       = createPDF(outFile, fontSize, orientation==LANDSCAPE)
    val startPage = new PageStartHandler(doc, orientation, "", "", headFont, headSize, startOnOdd)
    doc.getPdfDocument.addEventHandler(PdfDocumentEvent.START_PAGE, startPage)

    var files   = 0
    for (source<-sources) {
      {
        Console.err.println(source)
        val file     = new java.io.File(source)
        val modified = file.lastModified()
        val time     = LocalDateTime.ofEpochSecond(modified / 1000, 0, java.time.ZoneOffset.UTC)

        val para = new Paragraph().setFont(bodyFont).setFontSize(fontSize).setFixedLeading(fontSize)
        startPage.leftHead   = source
        startPage.pageNumber = 0
        startPage.leftFoot =
          time.format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE)+" "+
          time.format(java.time.format.DateTimeFormatter.ISO_LOCAL_TIME)
        // break pages between files
        if (files>0) doc.add(new AreaBreak(AreaBreakType.NEXT_PAGE))
        files += 1
        //
        val sourceFile = Source.fromFile(source, inputEnc)
        for (raw<-sourceFile.getLines())
        { val line = detab(raw)
          var indent = 0
          val length = line.length
          while (indent<length && line.charAt(indent)==' ') { indent += 1 }
          if (indent>0)      para.add(Indent(indent))
          if (indent<length) para.add(line.substring(indent))
          para.add("\n")
        }
        sourceFile.close()
        doc.add(para)
      }
    }
    doc.add(new AreaBreak(AreaBreakType.LAST_PAGE))
    Console.printf("%s: %d pages\n", outFile, doc.getPdfDocument.getNumberOfPages)
    doc.close()
  }
}
