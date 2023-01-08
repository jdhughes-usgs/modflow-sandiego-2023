ptf #
BEGIN OPTIONS
  SAVE_FLOWS
END OPTIONS

BEGIN GRIDDATA
  ICELLTYPE LAYERED
    CONSTANT 1
    CONSTANT 0
    CONSTANT 0
    CONSTANT 0
    CONSTANT 0
  K LAYERED
    OPEN/CLOSE k_aq.ref
    OPEN/CLOSE k_aq.ref
    OPEN/CLOSE k_clay.ref IPRN 5
    OPEN/CLOSE k_aq.ref
    OPEN/CLOSE k_aq.ref    
  K33 LAYERED
    OPEN/CLOSE k_aq.ref FACTOR #aqfact    #
    OPEN/CLOSE k_aq.ref FACTOR #aqfact    #
    OPEN/CLOSE k_clay.ref IPRN 5
    OPEN/CLOSE k_aq.ref FACTOR #aqfact    #
    OPEN/CLOSE k_aq.ref FACTOR #aqfact    #   
END GRIDDATA
