structure MipsFrame : FRAME = 
struct
    datatype access = InFrame of int 
                    | InReg of Temp.temp

end