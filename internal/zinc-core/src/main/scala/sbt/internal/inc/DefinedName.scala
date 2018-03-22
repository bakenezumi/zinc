/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package sbt.internal.inc

import xsbti.Position

case class DefinedName private (name: String, pos: Option[Position])
