((TA (INIT-ACTION) :start 0 :end 0 :dt 0)
 (TA (AC* SLIDE-BASE-IN (B-1 CARRY-IN TABLE-IN)) :start 0 :end 1 :dt 1)
 (TA (AC* EJECT-BASE (B-1 ARM1 TABLE-IN)) :start 2 :end 3 :dt 1)
 (TA (AC* MOVE-ARM-HOLDING
          (ARM1 TABLE-IN GASKET-MACHINE B-1)) :start 3 :end 5 :dt 2)
 (TA (AC* SET-BASE (B-1 ARM1 GASKET-MACHINE)) :start 5 :end 6 :dt 1)
 (TA (AC* ASSEMBLE-WITH-MACHINE
          (B-1 GASKET-MACHINE INSERT-GASKET
           NOTHING-DONE)) :start 6 :end 9 :dt 3)
 (TA (AC* EJECT-BASE (B-1 ARM1 GASKET-MACHINE)) :start 9 :end 10 :dt 1)
 (TA (AC* MOVE-ARM-HOLDING
          (ARM1 GASKET-MACHINE TABLE1 B-1)) :start 10 :end 12 :dt 2)
 (TA (AC* SET-BASE (B-1 ARM1 TABLE1)) :start 12 :end 13 :dt 1)
 (TA (AC* ASSEMBLE-WITH-ARM
          (PART-A ATTATCH-A INSERT-GASKET B-1 ARM1
           TABLE1)) :start 22 :end 25 :dt 3)
 (TA (AC* EJECT-BASE (B-1 ARM2 TABLE1)) :start 32 :end 33 :dt 1)
 (TA (AC* MOVE-ARM-HOLDING
          (ARM2 TABLE1 SCREW-MACHINE-A B-1)) :start 33 :end 35 :dt 2)
 (TA (AC* SET-BASE (B-1 ARM2 SCREW-MACHINE-A)) :start 35 :end 36 :dt 1)
 (TA (AC* ASSEMBLE-WITH-MACHINE
          (B-1 SCREW-MACHINE-A SCREW-A ATTATCH-A)) :start 36 :end 39 :dt 3)
 (TA (AC* EJECT-BASE (B-1 ARM2 SCREW-MACHINE-A)) :start 38 :end 39 :dt 1)
 (TA (AC* MOVE-ARM-HOLDING
          (ARM2 SCREW-MACHINE-A OILING-MACHINE B-1)) :start 39 :end 42 :dt 3)
 (TA (AC* SET-BASE (B-1 ARM2 OILING-MACHINE)) :start 42 :end 43 :dt 1)
 (TA (AC* ASSEMBLE-WITH-MACHINE
          (B-1 OILING-MACHINE OIL-CYLINDER SCREW-A)) :start 43 :end 46 :dt 3)
 (TA (AC* EJECT-BASE (B-1 ARM2 OILING-MACHINE)) :start 46 :end 47 :dt 1)
 (TA (AC* MOVE-ARM-HOLDING
          (ARM2 OILING-MACHINE TABLE2 B-1)) :start 47 :end 51 :dt 4)
 (TA (AC* SET-BASE (B-1 ARM2 TABLE2)) :start 51 :end 52 :dt 1)
 (TA (AC* ASSEMBLE-WITH-ARM
          (PART-B ATTATCH-B OIL-CYLINDER B-1 ARM2
           TABLE2)) :start 63 :end 65 :dt 2)
 (TA (AC* ASSEMBLE-WITH-ARM
          (PART-C ATTATCH-C ATTATCH-B B-1 ARM2
           TABLE2)) :start 72 :end 75 :dt 3)
 (TA (AC* EJECT-BASE (B-1 ARM2 TABLE2)) :start 75 :end 76 :dt 1)
 (TA (AC* MOVE-ARM-HOLDING
          (ARM2 TABLE2 SCREW-MACHINE-C B-1)) :start 76 :end 78 :dt 2)
 (TA (AC* SET-BASE (B-1 ARM2 SCREW-MACHINE-C)) :start 78 :end 79 :dt 1)
 (TA (AC* ASSEMBLE-WITH-MACHINE
          (B-1 SCREW-MACHINE-C SCREW-C ATTATCH-C)) :start 79 :end 81 :dt 2)
 (TA (AC* EJECT-BASE (B-1 ARM2 SCREW-MACHINE-C)) :start 81 :end 82 :dt 1)
 (TA (AC* MOVE-ARM-HOLDING
          (ARM2 SCREW-MACHINE-C TABLE2 B-1)) :start 82 :end 84 :dt 2)
 (TA (AC* SET-BASE (B-1 ARM2 TABLE2)) :start 84 :end 85 :dt 1)
 (TA (AC* EJECT-BASE (B-1 ARM1 TABLE2)) :start 95 :end 96 :dt 1)
 (TA (AC* MOVE-ARM-HOLDING
          (ARM1 TABLE2 INSPECTION-MACHINE B-1)) :start 96 :end 98 :dt 2)
 (TA (AC* SET-BASE (B-1 ARM1 INSPECTION-MACHINE)) :start 98 :end 99 :dt 1)
 (TA (AC* ASSEMBLE-WITH-MACHINE
          (B-1 INSPECTION-MACHINE INSPECT-BASE
           SCREW-C)) :start 99 :end 102 :dt 3)
 (TA (AC* EJECT-BASE (B-1 ARM1 INSPECTION-MACHINE)) :start 102 :end 103 :dt 1)
 (TA (AC* MOVE-ARM-HOLDING
          (ARM1 INSPECTION-MACHINE TABLE-OUT B-1)) :start 103 :end 105 :dt 2)
 (TA (AC* SET-BASE (B-1 ARM1 TABLE-OUT)) :start 105 :end 106 :dt 1)
 (TA (AC* SLIDE-BASE-OUT (B-1 TABLE-OUT CARRY-OUT)) :start 106 :end 107 :dt 1))