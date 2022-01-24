/*
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package faces.apps

import java.awt.Dimension
import java.io.{File, IOException}

import javax.swing._
import javax.swing.event.{ChangeEvent, ChangeListener}
import breeze.linalg.{DenseMatrix, DenseVector, inv, min}
import scalismo.faces.gui.{GUIBlock, GUIFrame, ImagePanel}
import scalismo.faces.gui.GUIBlock._
import scalismo.faces.parameters.RenderParameter
import scalismo.faces.io.{MeshIO, MoMoIO, PixelImageIO, RenderParameterIO}
import scalismo.faces.sampling.face.MoMoRenderer
import scalismo.color.RGB
import scalismo.faces.image.{AccessMode, PixelImage}
import scalismo.utils.Random
import scalismo.color.RGBA
import scalismo.faces.image.filter.IsotropicGaussianFilter
import scalismo.faces.momo.MoMo
import scalismo.statisticalmodel.MultivariateNormalDistribution
import scalismo.faces.parameters.{ParametricRenderer, RenderParameter, SphericalHarmonicsLight}
import scalismo.geometry.EuclideanVector3D

import scala.reflect.io.Path
import scala.util.{Failure, Try}

object MooneyFaceGenerator extends App {

  final val DEFAULT_DIR = new File(".")

  val modelFile: Option[File] = getModelFile(args)
  modelFile.map(MooneyFaceGeneratorGUI(_))

  private def getModelFile(args: Seq[String]): Option[File] = {
    if (args.nonEmpty) {
      val path = Path(args.head)
      if (path.isFile) return Some(path.jfile)
      if (path.isDirectory) return askUserForModelFile(path.jfile)
    }
    askUserForModelFile(DEFAULT_DIR)
  }

  private def askUserForModelFile(dir: File): Option[File] = {
    val jFileChooser = new JFileChooser(dir)
    if (jFileChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
      Some(jFileChooser.getSelectedFile)
    } else {
      println("No model select...")
      None
    }
  }
}

case class MooneyFaceGeneratorGUI(
                                   modelFile: File,
                                   imageWidth: Int = 512,
                                   imageHeight: Int = 512,
                                   maximalSliderValue: Int = 2,
                                   maximalShapeRank: Option[Int] = None,
                                   maximalColorRank: Option[Int] = None,
                                   maximalExpressionRank: Option[Int] = None
                                 ) {

  scalismo.initialize()
  val seed = 1024L
  implicit val rnd: Random = Random(seed)

  val model: MoMo = MoMoIO.read(modelFile, "").get
  var showExpressionModel: Boolean = model.hasExpressions
  var thr = 0.5
  var blur = 3

  val shapeRank: Int = maximalShapeRank match {
    case Some(rank) => min(model.neutralModel.shape.rank, rank)
    case _ => model.neutralModel.shape.rank
  }

  val colorRank: Int = maximalColorRank match {
    case Some(rank) => min(model.neutralModel.color.rank, rank)
    case _ => model.neutralModel.color.rank
  }

  val expRank: Int = maximalExpressionRank match {
    case Some(rank) => try{min(model.expressionModel.get.expression.rank, rank)} catch {case _: Exception => 0}
    case _ => try{model.expressionModel.get.expression.rank} catch {case _: Exception => 0}
  }

  // for illumination
  val dir = "data/parameters"
  val files = new File(dir).listFiles.filter(_.getName.endsWith(".rps")).toIndexedSeq
  var allIllumination = files.map(f => {
    val rps = RenderParameterIO.read(f).get
    rps.environmentMap
  })

  var allIlluminationData = allIllumination.map(i => i.toBreezeVector)
  val mnd = MultivariateNormalDistribution.estimateFromData(allIlluminationData)
  val pcs = mnd.principalComponents


  // vector holding the coefficients
  var coeffs = DenseVector.fill(pcs.length){0.0}
  // scaling factor for sliders
  val sigFactor: Int = 100
  // the maximal sigma the sliders should reach
  var maximalSigma: Int = 3

  val mooneyRank: Int = 2

  var renderer: MoMoRenderer = MoMoRenderer(model, RGBA.BlackTransparent).cached(5)
  // a matrix containing all eigenvectors
  val pcM = DenseMatrix(pcs.map(p => p._1).toArray:_*).t
  val inverse_pcM=inv(pcM)
  // a vector containing all squareroots of the eigenvalues to scale the coefficients
  val sqrtEV = DenseVector(pcs.map(p => Math.sqrt(p._2)):_*)

  // Changed to my illumination
  val rndIllumminationPath = files(rnd.scalaRandom.nextInt(files.length))

  // Get illumination from coefficients and add illumination to set of renderParameters
  def addIllumination(c : DenseVector[Double])  = {
    //System.out.println("coeffs: " + c)
    val scaledC: DenseVector[Double] = c *:* sqrtEV
    //System.out.println("scaledC: " + scaledC)
    val sHcoeffs: DenseVector[Double] = mnd.mean + pcM * scaledC
    //System.out.println("sHcoeffs: " + sHcoeffs)
    val newIllumination = SphericalHarmonicsLight.fromBreezeVector(sHcoeffs)
    //System.out.println("out: " + newIllumination)
    init = init.copy(environmentMap = newIllumination)
  }

  def backaddIllumination()  = {
    val sHcoeffs = init.environmentMap.toBreezeVector
    val scaledC = inverse_pcM*(sHcoeffs - mnd.mean)
    coeffs = (1.0/sqrtEV) *:* scaledC
  }

  val initDefault: RenderParameter = RenderParameter.defaultSquare.fitToImageSize(imageWidth,imageHeight)
  var init: RenderParameter = initDefault.copy(
    momo = initDefault.momo.withNumberOfCoefficients(shapeRank, colorRank, expRank),
    pose = initDefault.pose.copy(translation = EuclideanVector3D(0.0, 0.0, -1200.0))
  )
  addIllumination(coeffs)

  var changingSliders = false

  val sliderSteps = 1000
  var maximalSigmaSpinner: JSpinner = {
    val spinner = new JSpinner(new SpinnerNumberModel(maximalSigma,0,999,1))
    spinner.addChangeListener( new ChangeListener() {
      override def stateChanged(e: ChangeEvent): Unit = {
        val newMaxSigma = spinner.getModel.asInstanceOf[SpinnerNumberModel].getNumber.intValue()
        maximalSigma = math.abs(newMaxSigma)
        setShapeSliders()
        setColorSliders()
        setExpSliders()
        setIllSliders()
        setMooneySliders((thr*100-50).toInt, blur*5-25)
      }
    })
    spinner.setToolTipText("maximal slider value")
    spinner
  }


  def sliderToParam(value: Int): Double = {
    maximalSigma * value.toDouble/sliderSteps
  }

  def paramToSlider(value: Double): Int = {
    (value / maximalSigma * sliderSteps).toInt
  }

  val bg = PixelImage(imageWidth, imageHeight, (_, _) => RGBA.Black)

  val imageWindow = ImagePanel(renderMooney(init))
  val imageWindoworig = ImagePanel(renderRGB(init))

  //--- SHAPE -----
  val shapeSlider: IndexedSeq[JSlider] = for (n <- 0 until shapeRank) yield {
    GUIBlock.slider(-sliderSteps, sliderSteps, 0, f => {
      updateShape(n, f)
      updateImage()
    })
  }

  val shapeSliderView: JPanel = GUIBlock.shelf(shapeSlider.zipWithIndex.map(s => GUIBlock.stack(s._1, new JLabel("" + s._2))): _*)
  val shapeScrollPane = new JScrollPane(shapeSliderView)
  val shapeScrollBar: JScrollBar = shapeScrollPane.createVerticalScrollBar()
  shapeScrollPane.setSize(800, 300)
  shapeScrollPane.setPreferredSize(new Dimension(800, 300))

  val rndShapeButton: JButton = GUIBlock.button("random", {
    randomShape(); updateImage()
  })
  val resetShapeButton: JButton = GUIBlock.button("reset", {
    resetShape(); updateImage()
  })
  rndShapeButton.setToolTipText("draw each shape parameter at random from a standard normal distribution")
  resetShapeButton.setToolTipText("set all shape parameters to zero")

  def updateShape(n: Int, value: Int): Unit = {
    init = init.copy(momo = init.momo.copy(shape = {
      val current = init.momo.shape
      current.zipWithIndex.map { case (v, i) => if (i == n) sliderToParam(value) else v }
    }))
  }

  def randomShape(): Unit = {
    init = init.copy(momo = init.momo.copy(shape = {
      val current = init.momo.shape
      current.zipWithIndex.map {
        case (_, _) =>
          rnd.scalaRandom.nextGaussian
      }

    }))
    setShapeSliders()
  }

  def resetShape(): Unit = {
    init = init.copy(momo = init.momo.copy(
      shape = IndexedSeq.fill(shapeRank)(0.0)
    ))
    setShapeSliders()
  }

  def setShapeSliders(): Unit = {
    changingSliders = true
    (0 until shapeRank).foreach(i => {
      shapeSlider(i).setValue(paramToSlider(init.momo.shape(i)))
    })
    changingSliders = false
  }
  // Illumination

  val illuminationSlider: IndexedSeq[JSlider] = for (n <- 0 until pcs.length) yield {
    GUIBlock.slider(-maximalSigma * sigFactor, maximalSigma * sigFactor, 0, f => {
      updateIll(n, f)
      updateImage()
    })
  }

  val illSliderView: JPanel = GUIBlock.shelf(illuminationSlider.zipWithIndex.map(s => GUIBlock.stack(s._1, new JLabel("" + s._2))): _*)
  val illScrollPane = new JScrollPane(illSliderView)
  val illScrollBar: JScrollBar = illScrollPane.createVerticalScrollBar()
  illScrollPane.setSize(800, 300)
  illScrollPane.setPreferredSize(new Dimension(800, 300))

  val rndIllButton: JButton = GUIBlock.button("random", {
    randomIllumination(); updateImage()
  })
  val resetIllButton: JButton = GUIBlock.button("reset", {
    resetIllumination(); updateImage()
  })
  rndIllButton.setToolTipText("draw each illumination parameter at random from a standard normal distribution")
  resetIllButton.setToolTipText("set all illumination parameters to zero")

  def updateIll(n: Int, value: Int): Unit = {
    coeffs(n) = value.toDouble / sigFactor
    addIllumination(coeffs)
  }

  def setIllSliders() = {
    changingSliders = true
    (0 until pcs.length).foreach(i => {
      illuminationSlider(i).setValue(Math.round(coeffs(i) * sigFactor).toInt)
    })
    changingSliders = false
    addIllumination(coeffs)
  }

  def randomIllumination() = {
    coeffs = DenseVector.fill(pcs.length){rnd.scalaRandom.nextGaussian}
    setIllSliders()
  }

  def resetIllumination() = {
    coeffs = DenseVector.fill(pcs.length){0.0}
    setIllSliders()
  }

  //--- MOONEY -----
  val mooneySlider: IndexedSeq[JSlider] = for (n <- 0 until mooneyRank) yield {
    GUIBlock.slider(-50, 50, 0, f => {
      updateMooney(n, f+50)
      updateImage()
    })
  }

  def name(x:Int): String = {
    if (x == 0)
      return "threshold"
    else
      return "blur"
  }

  def mooneyfy(imgIn: PixelImage[RGBA], thr:Double, blur:Int): PixelImage[RGBA]={
    var cnt = 0;

    val white = imgIn.map(p => {
      if (p.a>.5)
        RGBA(1.0)
      else
        RGBA(0.0)
    })
    val whitemean = scalismo.faces.image.PixelImageOperations.mean(white)

    val img = imgIn.map(_.gray)
    val filteredImage = img.withAccessMode(AccessMode.Repeat[Double]).filter(IsotropicGaussianFilter(35, 2*blur+1))



    // binary search on the threshold needed to get the specified percent of white pixels
    var a = 0.0
    var b = 1.0
    while (b-a > .001) {
      val mid = (a+b)/2.0
      // create a new image with mid as the threshold
      val temp = filteredImage.map(p => {
        if (p>mid)
          RGBA(1.0)
        else
          RGBA(0.0)
      })
      // find the percent of white pixels
      val x = scalismo.faces.image.PixelImageOperations.mean(temp)
      // lower number = more black pixels
      // update the bounds of binary search based on these values
      if (x.r/whitemean.r > thr) a = mid
      else b = mid
    }
    val thresholded = filteredImage.map(p => {
      if (p > a)
        RGBA(1.0)
      else
        RGBA(0.0)
    })

    return thresholded
  }


  val mooneySliderView: JPanel = GUIBlock.shelf(mooneySlider.zipWithIndex.map(s => GUIBlock.stack(s._1, new JLabel(name(s._2)))): _*)
  val mooneyScrollPane = new JScrollPane(mooneySliderView)
  val mooneyScrollBar: JScrollBar = mooneyScrollPane.createVerticalScrollBar()
  mooneyScrollPane.setSize(800, 300)
  mooneyScrollPane.setPreferredSize(new Dimension(800, 300))

  val rndMooneyButton: JButton = GUIBlock.button("random", {
    randomMooney(); updateImage()
  })
  val resetMooneyButton: JButton = GUIBlock.button("reset", {
    resetMooney(); updateImage()
  })
  rndMooneyButton.setToolTipText("draw each mooney parameter at random from a standard normal distribution")
  resetMooneyButton.setToolTipText("set all mooney parameters to starting values")

  def randomMooney(): Unit = {
    val n = rnd.scalaRandom.nextInt(50)-25
    val f = rnd.scalaRandom.nextInt(50)-25
    setMooneySliders(n, f)
    updateImage()

  }

  def resetMooney(): Unit = {
    thr = 0.5
    blur = 3
    setMooneySliders(0, 0)
  }

  def setMooneySliders(n: Int, f: Int): Unit = {
    changingSliders = true
    mooneySlider(0).setValue(n)
    mooneySlider(1).setValue(f)
    changingSliders = false
  }

  def updateMooney(n: Int, value: Int): Unit = {
    if (n == 0) {
      thr = value/100.0
      //System.out.println("thr: " + thr)
    }
    else {
      blur = value/5
    }
  }


  //--- COLOR -----
  val colorSlider: IndexedSeq[JSlider] = for (n <- 0 until colorRank) yield {
    GUIBlock.slider(-sliderSteps, sliderSteps, 0, f => {
      updateColor(n, f)
      updateImage()
    })
  }

  val colorSliderView: JPanel = GUIBlock.shelf(colorSlider.zipWithIndex.map(s => GUIBlock.stack(s._1, new JLabel("" + s._2))): _*)
  val colorScrollPane = new JScrollPane(colorSliderView)
  val colorScrollBar: JScrollBar = colorScrollPane.createHorizontalScrollBar()
  colorScrollPane.setSize(800, 300)
  colorScrollPane.setPreferredSize(new Dimension(800, 300))

  val rndColorButton: JButton = GUIBlock.button("random", {
    randomColor(); updateImage()
  })

  val resetColorButton: JButton = GUIBlock.button("reset", {
    resetColor(); updateImage()
  })


  rndColorButton.setToolTipText("draw each color parameter at random from a standard normal distribution")
  resetColorButton.setToolTipText("set all color parameters to zero")

  def updateColor(n: Int, value: Int): Unit = {
    init = init.copy(momo = init.momo.copy(color = {
      val current = init.momo.color
      current.zipWithIndex.map { case (v, i) => if (i == n) sliderToParam(value) else v }
    }))
  }

  def randomColor(): Unit = {
    init = init.copy(momo = init.momo.copy(color = {
      val current = init.momo.color
      current.zipWithIndex.map {
        case (_, _) =>
          rnd.scalaRandom.nextGaussian
      }

    }))
    setColorSliders()
  }

  def resetColor(): Unit = {
    init = init.copy(momo = init.momo.copy(
      color = IndexedSeq.fill(colorRank)(0.0)
    ))
    setColorSliders()
  }

  def setColorSliders(): Unit = {
    changingSliders = true
    (0 until colorRank).foreach(i => {
      colorSlider(i).setValue(paramToSlider(init.momo.color(i)))
    })
    changingSliders = false
  }

  //--- EXPRESSION -----
  val expSlider: IndexedSeq[JSlider] = for (n <- 0 until expRank)yield {
    GUIBlock.slider(-sliderSteps, sliderSteps, 0, f => {
      updateExpression(n, f)
      updateImage()
    })
  }

  val expSliderView: JPanel = GUIBlock.shelf(expSlider.zipWithIndex.map(s => GUIBlock.stack(s._1, new JLabel("" + s._2))): _*)
  val expScrollPane = new JScrollPane(expSliderView)
  val expScrollBar: JScrollBar = expScrollPane.createVerticalScrollBar()
  expScrollPane.setSize(800, 300)
  expScrollPane.setPreferredSize(new Dimension(800, 300))

  val rndExpButton: JButton = GUIBlock.button("random", {
    randomExpression(); updateImage()
  })
  val resetExpButton: JButton = GUIBlock.button("reset", {
    resetExpression(); updateImage()
  })

  rndExpButton.setToolTipText("draw each expression parameter at random from a standard normal distribution")
  resetExpButton.setToolTipText("set all expression parameters to zero")

  def updateExpression(n: Int, value: Int): Unit = {
    init = init.copy(momo = init.momo.copy(expression = {
      val current = init.momo.expression
      current.zipWithIndex.map { case (v, i) => if (i == n) sliderToParam(value) else v }
    }))
  }

  def randomExpression(): Unit = {
    init = init.copy(momo = init.momo.copy(expression = {
      val current = init.momo.expression
      current.zipWithIndex.map {
        case (_, _) =>
          rnd.scalaRandom.nextGaussian
      }

    }))
    setExpSliders()
  }

  def resetExpression(): Unit = {
    init = init.copy(momo = init.momo.copy(
      expression = IndexedSeq.fill(expRank)(0.0)
    ))
    setExpSliders()
  }

  def setExpSliders(): Unit = {
    changingSliders = true
    (0 until expRank).foreach(i => {
      expSlider(i).setValue(paramToSlider(init.momo.expression(i)))
    })
    changingSliders = false
  }

  //--- ALL TOGETHER -----
  val randomButton: JButton = GUIBlock.button("random", {
    randomShape(); randomColor(); randomExpression(); randomIllumination(); updateImage()
  })
  val resetButton: JButton = GUIBlock.button("reset", {
    resetShape(); resetColor(); resetExpression(); resetMooney(); resetIllumination(); updateImage();
  })
  val randomIll: JButton = GUIBlock.button("random illumination", {
    randomIllumination(); updateImage()
  })

  val toggleExpressionButton: JButton = GUIBlock.button("expressions off", {
    if ( model.hasExpressions ) {
      if ( showExpressionModel ) renderer = MoMoRenderer(model.neutralModel, RGBA.BlackTransparent).cached(5)
      else renderer = MoMoRenderer(model, RGBA.BlackTransparent).cached(5)

      showExpressionModel = !showExpressionModel
      updateToggleExpressioButton()
      addRemoveExpressionTab()
      updateImage()
    }
  })

  def updateToggleExpressioButton(): Unit = {
    if ( showExpressionModel ) toggleExpressionButton.setText("expressions off")
    else toggleExpressionButton.setText("expressions on")
  }

  randomButton.setToolTipText("draw each model parameter at random from a standard normal distribution")
  resetButton.setToolTipText("set all model parameters to zero")
  randomIll.setToolTipText("random illumination")
  toggleExpressionButton.setToolTipText("toggle expression part of model on and off")

  //function to export the current shown face as a .ply file
  def exportShape (): Try[Unit] ={

    def askToOverwrite(file: File): Boolean = {
      val dialogButton = JOptionPane.YES_NO_OPTION
      JOptionPane.showConfirmDialog(null, s"Would you like to overwrite the existing file: $file?","Warning",dialogButton) == JOptionPane.YES_OPTION
    }

    val VCM3D = if (model.hasExpressions && !showExpressionModel) model.neutralModel.instance(init.momo.coefficients)
    else model.instance(init.momo.coefficients)

    val fc = new JFileChooser()
    fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES)
    fc.setDialogTitle("Select a folder to store the .ply file and name it")
    if (fc.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {
      var file = fc.getSelectedFile
      if (file.isDirectory) file = new File(file,"instance.ply")
      if ( !file.getName.endsWith(".ply")) file = new File( file+".ply")
      if (!file.exists() || askToOverwrite(file)) {
        MeshIO.write(VCM3D, file)
      } else {
        Failure(new IOException(s"Something went wrong when writing to file the file $file."))
      }
    } else {
      Failure(new Exception("User aborted save dialog."))
    }
  }

  //function to export the current shown face as a .png file
  def exportImage (): Try[Unit] ={

    def askToOverwrite(file: File): Boolean = {
      val dialogButton = JOptionPane.YES_NO_OPTION
      JOptionPane.showConfirmDialog(null, s"Would you like to overwrite the existing file: $file?","Warning",dialogButton) == JOptionPane.YES_OPTION
    }

    val img2: PixelImage[RGBA] = renderer.renderImage(init) // change code here
    val img = mooneyfy(img2, thr, blur)


    val fc = new JFileChooser()
    fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES)
    fc.setDialogTitle("Select a folder to store the .png file and name it")
    if (fc.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {
      var file = fc.getSelectedFile
      if (file.isDirectory) file = new File(file,"instance.png")
      if ( !file.getName.endsWith(".png")) file = new File( file+".png")
      if (!file.exists() || askToOverwrite(file)) {
        PixelImageIO.write(img, file)
      } else {
        Failure(new IOException(s"Something went wrong when writing to file the file $file."))
      }
    } else {
      Failure(new Exception("User aborted save dialog."))
    }
  }

  // function to export the original image as .png
  def exportOriginalImage (): Try[Unit] ={

    def askToOverwrite(file: File): Boolean = {
      val dialogButton = JOptionPane.YES_NO_OPTION
      JOptionPane.showConfirmDialog(null, s"Would you like to overwrite the existing file: $file?","Warning",dialogButton) == JOptionPane.YES_OPTION
    }

    val img: PixelImage[RGBA] = renderer.renderImage(init) // change code here

    val fc = new JFileChooser()
    fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES)
    fc.setDialogTitle("Select a folder to store the .png file and name it")
    if (fc.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {
      var file = fc.getSelectedFile
      if (file.isDirectory) file = new File(file,"instance.png")
      if ( !file.getName.endsWith(".png")) file = new File( file+".png")
      if (!file.exists() || askToOverwrite(file)) {
        PixelImageIO.write(img, file)
      } else {
        Failure(new IOException(s"Something went wrong when writing to file the file $file."))
      }
    } else {
      Failure(new Exception("User aborted save dialog."))
    }
  }

  //exportShape button and its tooltip
  val exportShapeButton: JButton = GUIBlock.button("export PLY",
    {
      exportShape()
    }
  )
  exportShapeButton.setToolTipText("export the current shape and texture as .ply")

  //exportImage button and its tooltip
  val exportImageButton: JButton = GUIBlock.button("export PNG",
    {
      exportImage()
    }
  )
  exportImageButton.setToolTipText("export the current image as .png")

  //exportOriginalImage button and its tooltip
  val exportOriginalImageButton: JButton = GUIBlock.button("export original PNG",
    {
      exportOriginalImage()
    }
  )
  exportOriginalImageButton.setToolTipText("export the original image as .png")


  def askUserForRPSFile(dir: File): Option[File] = {
    val jFileChooser = new JFileChooser(dir)
    if (jFileChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
      Some(jFileChooser.getSelectedFile)
    } else {
      println("No Parameters select...")
      None
    }
  }

  def resizeParameterSequence(params: IndexedSeq[Double], length: Int, fill: Double): IndexedSeq[Double] = {
    val zeros = IndexedSeq.fill[Double](length)(fill)
    (params ++ zeros).slice(0, length) //brute force
  }

  def updateModelParameters(params: RenderParameter): Unit = {
    init = params
    backaddIllumination()
    setShapeSliders()
    setColorSliders()
    setExpSliders()
    setIllSliders()
    setMooneySliders((thr*100-50).toInt, blur*5-25)
    updateImage()
  }

  val loadMooneyButton: JButton = GUIBlock.button(
    "load mooney RPS",
    {
      for {rpsFile <- askUserForRPSFile(new File("."))
           rpsParams <- RenderParameterIO.read(rpsFile)} {
        var name:String = rpsFile.toString()
        var use = name.split("_")

        thr = use(1).split("=")(1).toDouble
        blur = use(2).split("=")(1).toInt

        val maxSigma = (rpsParams.momo.shape ++ rpsParams.momo.color ++ rpsParams.momo.expression).map(math.abs).max
        if ( maxSigma > maximalSigma ) {
          maximalSigma = math.ceil(maxSigma).toInt
          maximalSigmaSpinner.setValue(maximalSigma)
          setShapeSliders()
          setColorSliders()
          setExpSliders()
          setIllSliders()
          setMooneySliders((thr*100-50).toInt, blur*5-25)
        }
        updateModelParameters(rpsParams)
        //System.out.println(init.momo)
        //System.out.println(init.environmentMap)
        //System.out.println(init.momo.expression)
      }
    }
  )

  val writeMooneyButton: JButton = GUIBlock.button(
    "write mooney RPS",
    {
      def askToOverwrite(file: File): Boolean = {
        val dialogButton = JOptionPane.YES_NO_OPTION
        JOptionPane.showConfirmDialog(null, s"Would you like to overwrite the existing file: $file?","Warning",dialogButton) == JOptionPane.YES_OPTION
      }

      val fc = new JFileChooser()
      fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES)
      fc.setDialogTitle("Select a folder to store the .rps file and name it. Don't use '_' in the name.")
      if (fc.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {
        var file = fc.getSelectedFile
        if (file.isDirectory) file = new File(file,"instance.rps")
        file = new File( file+"_thr=" + thr + "_blur=" + blur + "_.rps")
        if (!file.exists() || askToOverwrite(file)) {
          RenderParameterIO.write(init, file)
        } else {
          Failure(new IOException(s"Something went wrong when writing to file the file $file."))
        }
      } else {
        Failure(new Exception("User aborted save dialog."))
      }
    }
  )


  //---- update the image
  def updateImage(): Unit = {
    if (!changingSliders) {
      imageWindow.updateImage(renderMooney(init))
      imageWindoworig.updateImage(renderRGB(init))
    }
  }

  def renderMooney(init: RenderParameter): PixelImage[RGB] = {
    val fg2 = renderer.renderImage(init)
    val fg = mooneyfy(fg2, thr, blur)
    fg.zip(bg).map { case (f, b) => b.toRGB.blend(f) }
  }

  def renderRGB(init: RenderParameter): PixelImage[RGB] = {
    val fg = renderer.renderImage(init)
    fg.zip(bg).map { case (f, b) => b.toRGB.blend(f) }
    //    fg.map(_.toRGB)
  }

  //--- COMPOSE FRAME ------
  val controls = new JTabbedPane()
  controls.addTab("color", GUIBlock.stack(colorScrollPane, GUIBlock.shelf(rndColorButton, resetColorButton)))
  controls.addTab("shape", GUIBlock.stack(shapeScrollPane, GUIBlock.shelf(rndShapeButton, resetShapeButton)))
  if ( model.hasExpressions)
    controls.addTab("expression", GUIBlock.stack(expScrollPane, GUIBlock.shelf(rndExpButton, resetExpButton)))
  controls.addTab("mooney", GUIBlock.stack(mooneyScrollPane, GUIBlock.shelf(rndMooneyButton, resetMooneyButton)))
  controls.addTab("illumination", GUIBlock.stack(illScrollPane, GUIBlock.shelf(rndIllButton, resetIllButton)))
  def addRemoveExpressionTab(): Unit = {
    if ( showExpressionModel ) {
      controls.addTab("expression", GUIBlock.stack(expScrollPane, GUIBlock.shelf(rndExpButton, resetExpButton)))
    } else {
      val idx = controls.indexOfTab("expression")
      if ( idx >= 0) controls.remove(idx)
    }
  }

  val guiFrame: GUIFrame = GUIBlock.stack(
    GUIBlock.shelf(GUIBlock.stack(imageWindoworig, imageWindow),
      GUIBlock.stack(controls,
        if (model.hasExpressions) {
          GUIBlock.shelf(maximalSigmaSpinner, randomButton, randomIll, resetButton, toggleExpressionButton, loadMooneyButton, writeMooneyButton, exportShapeButton, exportImageButton, exportOriginalImageButton)
        } else {
          GUIBlock.shelf(maximalSigmaSpinner, randomButton, randomIll, resetButton, loadMooneyButton, writeMooneyButton, exportShapeButton, exportImageButton, exportOriginalImageButton)
        }
      )
    )
  ).displayIn("Parametric Mooney Face Generator")


  //--- ROTATION CONTROLS ------

  import java.awt.event._

  var lookAt = false
  imageWindow.requestFocusInWindow()

  imageWindow.addKeyListener(new KeyListener {
    override def keyTyped(e: KeyEvent): Unit = {
    }

    override def keyPressed(e: KeyEvent): Unit = {
      if (e.getKeyCode == KeyEvent.VK_CONTROL) lookAt = true
    }

    override def keyReleased(e: KeyEvent): Unit = {
      if (e.getKeyCode == KeyEvent.VK_CONTROL) lookAt = false
    }
  })

  imageWindow.addMouseListener(new MouseListener {
    override def mouseExited(e: MouseEvent): Unit = {}

    override def mouseClicked(e: MouseEvent): Unit = {
      imageWindow.requestFocusInWindow()
    }

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mousePressed(e: MouseEvent): Unit = {}

    override def mouseReleased(e: MouseEvent): Unit = {}
  })

  imageWindow.addMouseMotionListener(new MouseMotionListener {
    override def mouseMoved(e: MouseEvent): Unit = {
      if (lookAt) {
        val x = e.getX
        val y = e.getY
        val yawPose = math.Pi / 2 * (x - imageWidth * 0.5) / (imageWidth / 2)
        val pitchPose = math.Pi / 2 * (y - imageHeight * 0.5) / (imageHeight / 2)

        init = init.copy(pose = init.pose.copy(yaw = yawPose, pitch = pitchPose))
        updateImage()
      }
    }

    override def mouseDragged(e: MouseEvent): Unit = {}
  })


  var lookAtorig = false
  imageWindoworig.requestFocusInWindow()

  imageWindoworig.addKeyListener(new KeyListener {
    override def keyTyped(e: KeyEvent): Unit = {
    }

    override def keyPressed(e: KeyEvent): Unit = {
      if (e.getKeyCode == KeyEvent.VK_CONTROL) lookAtorig = true
    }

    override def keyReleased(e: KeyEvent): Unit = {
      if (e.getKeyCode == KeyEvent.VK_CONTROL) lookAtorig = false
    }
  })

  imageWindoworig.addMouseListener(new MouseListener {
    override def mouseExited(e: MouseEvent): Unit = {}

    override def mouseClicked(e: MouseEvent): Unit = {
      imageWindoworig.requestFocusInWindow()
    }

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mousePressed(e: MouseEvent): Unit = {}

    override def mouseReleased(e: MouseEvent): Unit = {}
  })

  imageWindoworig.addMouseMotionListener(new MouseMotionListener {
    override def mouseMoved(e: MouseEvent): Unit = {
      if (lookAtorig) {
        val x = e.getX
        val y = e.getY
        val yawPose = math.Pi / 2 * (x - imageWidth * 0.5) / (imageWidth / 2)
        val pitchPose = math.Pi / 2 * (y - imageHeight * 0.5) / (imageHeight / 2)

        init = init.copy(pose = init.pose.copy(yaw = yawPose, pitch = pitchPose))
        updateImage()
      }
    }

    override def mouseDragged(e: MouseEvent): Unit = {}
  })

}
