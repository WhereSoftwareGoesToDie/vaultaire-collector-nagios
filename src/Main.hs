-- This file is part of vaultaire-collector-nagios.
--
-- Copyright 2014 Anchor Systems Pty Ltd and others.
-- 
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under the terms of the BSD license.

module Main where

import Data.Nagios.Perfdata.Collector.State

main :: IO ()
main = runCollector
