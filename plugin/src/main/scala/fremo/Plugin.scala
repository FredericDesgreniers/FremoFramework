package fremo

import dotty.tools.dotc.config.Settings.Setting
import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import SymDenotations.*
import ast.tpd
import ast.Trees._

import StdNames.nme
import Names._
import Constants.Constant

import scala.language.implicitConversions

class Plugin extends StandardPlugin:
  override def description: String = "Fremo runtime plugin"

  override def name: String = "Fremo Plugin"

  override def init(options: List[String]): List[PluginPhase] =
    (new PhaseA() :: Nil)


class PhaseA() extends PluginPhase:
  import tpd._
  override def phaseName: String = "Phase A"
  override def runsAfter: Set[String] = Set(transform.Pickler.name)
  override def runsBefore: Set[String] = Set(transform.Erasure.name)

  override def prepareForUnit(tree: tpd.Tree)(using Context): Context =
    ctx

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    val sym = tree.symbol

    report.error("This will always error out", tree.srcPos)

    if tree.rhs.isEmpty || sym.isOneOf(Synthetic | Deferred | Private | Accessor) then return tree

    tree