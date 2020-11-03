import com.itextpdf.kernel.events.{Event, IEventHandler, PdfDocumentEvent}
import com.itextpdf.kernel.font.PdfFont
import com.itextpdf.kernel.pdf.PdfNumber
import com.itextpdf.kernel.pdf.canvas.PdfCanvas
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.AreaBreak
import com.itextpdf.layout.property.AreaBreakType

object Utilities
{

  def textWidth(text: String, font: PdfFont, fontSize: Float): Float =
  {
    font.getWidth(text) * fontSize / 1000f
  }

  /** Expand tabs within the given string */
  def detab(in: String) : String =
    if (in.contains('\t'))
    {
      val parts = in.split('\t')
      val out   = for (part<-parts) yield part + " "*(8 - (part.length % 8))
      out.mkString("")
    } else in

  class PageStartHandler(val doc        : Document,
                         var orientation: PdfNumber = new PdfNumber(0),
                         var leftHead   : String="",
                         var leftFoot   : String="",
                         val font       : PdfFont = null,
                         val fontSize   : Float = 10f,
                         val startOnOdd : Boolean = true)  extends IEventHandler
  {
    var pageNumber =  0
    val NEXTPAGE   =  new AreaBreak(AreaBreakType.NEXT_PAGE)

    def handleEvent(event: Event): Unit =
    {
      val docEvent = event.asInstanceOf[PdfDocumentEvent]
      val page = docEvent.getPage
      val pdf  = docEvent.getDocument

      // Make a little centered heading
      val listingPage = pdf.getPageNumber(page)
      // start file on an odd page, so that long listings can be modular
      if (startOnOdd && pageNumber==0 && listingPage % 2 == 0)
      {
        doc.add(NEXTPAGE)
      }
      else
      {
        pageNumber += 1
        val pageSize = page.getPageSize
        val pdfCanvas = new PdfCanvas(page.getLastContentStream, page.getResources, pdf)

        val rightHead = s"Page $pageNumber"
        val ascent = font.getAscent(rightHead, fontSize)
        val descent = font.getDescent(rightHead, fontSize)
        val height = ascent + descent
        val headY = pageSize.getTop - doc.getTopMargin + 4
        val footY = pageSize.getBottom + doc.getBottomMargin
        val rightWidth = textWidth(rightHead, font, fontSize)

        pdfCanvas.saveState()
        .moveTo(doc.getLeftMargin - 2, headY)
        .lineTo(pageSize.getWidth - doc.getRightMargin + 2, headY)
        .stroke()
        .restoreState()


        pdfCanvas.beginText()
        .setFontAndSize(font, fontSize)
        .moveText(doc.getLeftMargin, headY + height)
        .showText(leftHead)
        .endText()
        .beginText()
        .moveText(pageSize.getWidth - doc.getRightMargin - rightWidth, headY + height)
        .showText(rightHead)
        .endText()
        .beginText()
        .moveText(doc.getLeftMargin, footY - height - 4)
        .showText(leftFoot)
        .endText()

        pdfCanvas.saveState()
        .moveTo(doc.getLeftMargin - 2, footY)
        .lineTo(pageSize.getWidth - doc.getRightMargin + 2, footY)
        .stroke()
        .restoreState()

        pdfCanvas.release()
      }

    }
  }
}
