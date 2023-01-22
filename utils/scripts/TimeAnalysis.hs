{-# LANGUAGE OverloadedStrings #-}

-- module  TimeAnalysis where

import           Ledger          (POSIXTime (POSIXTime), Slot (Slot),
                                  getPOSIXTime, getSlot)
import           Ledger.TimeSlot (SlotConfig (SlotConfig),
                                  posixTimeToEnclosingSlot, slotToEndPOSIXTime)
import           Prelude


beginningOfTestnetTime :: POSIXTime
beginningOfTestnetTime = POSIXTime 1655683180000

prePondTestnetConf :: SlotConfig
prePondTestnetConf = SlotConfig 1000 beginningOfTestnetTime

intPOSIXTimeToSlot :: Integer -> Slot
intPOSIXTimeToSlot = posixTimeToEnclosingSlot prePondTestnetConf . POSIXTime

intSlotToPOSIXTime :: Integer -> POSIXTime
intSlotToPOSIXTime = slotToEndPOSIXTime prePondTestnetConf . Slot
Reference
main :: IO ()
main = do
        let
            currentDateInPOSIXTime  = 1673444043 * 1000
            currentDateOfNodeInSlot = 17760817
            phaseOneDateInPOSIXTime = currentDateInPOSIXTime + 600000
            phaseTwoDateInPOSIXTime = currentDateInPOSIXTime + 1200000

        putStrLn        "\n<--------------------------------------DONE------------------------------------------>"
        putStrLn    $   "       Phase One Date In POSIXTime (ms):               " <> show phaseOneDateInPOSIXTime
        putStrLn    $   "       Phase One Date In Slot (s):                     " <> show (getSlot $ intPOSIXTimeToSlot phaseOneDateInPOSIXTime)
        putStrLn        ""
        putStrLn    $   "       Phase Two Date In POSIXTime (ms):               " <> show phaseTwoDateInPOSIXTime
        putStrLn    $   "       Phase Two Date In Slot (s):                     " <> show (getSlot $ intPOSIXTimeToSlot phaseTwoDateInPOSIXTime)
        putStrLn        ""
        putStrLn    $   "       Current Date In POSIXTime (ms):                 " <> show currentDateInPOSIXTime
        putStrLn    $   "       Current Date In Slot (s):                       " <> show (getSlot $ intPOSIXTimeToSlot currentDateInPOSIXTime)
        putStrLn        ""
        putStrLn    $   "       Current Date Of Node In POSIXTime (ms):         " <> show (getPOSIXTime $ intSlotToPOSIXTime currentDateOfNodeInSlot)
        putStrLn    $   "       Current Date Of Node In Slot (s):               " <> show currentDateOfNodeInSlot
        putStrLn        ""
        putStrLn    $   "       Phase One Diff With Node In POSIXTime (ms):     " <> show  (phaseOneDateInPOSIXTime - getPOSIXTime (intSlotToPOSIXTime currentDateOfNodeInSlot))
        putStrLn    $   "       Phase One Diff With Node In Slot (s):           " <> show  (getSlot (intPOSIXTimeToSlot phaseOneDateInPOSIXTime) - currentDateOfNodeInSlot )
        putStrLn        ""
        putStrLn    $   "       Phase Two Diff With Node In POSIXTime (ms):     " <> show  (phaseTwoDateInPOSIXTime - getPOSIXTime (intSlotToPOSIXTime currentDateOfNodeInSlot))
        putStrLn    $   "       Phase Two Diff With Node In Slot (s):           " <> show  (getSlot (intPOSIXTimeToSlot phaseTwoDateInPOSIXTime) - currentDateOfNodeInSlot )
        putStrLn        "<------------------------------------------------------------------------------------>\n"
