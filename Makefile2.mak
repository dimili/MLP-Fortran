FLAGS = /O3
#FLAGS  = /O3          
#FLAGS  = /Zi
EXE     = UseMBP
OBJ     = UseNetwork.obj Passes.obj ActivateFunctions.obj

$(EXE): $(OBJ)
	ifort  $(FLAGS) $(OBJ) /o $(EXE)
#
UseNetwork.obj: UseNetwork.f
	ifort  $(FLAGS)     /c  UseNetwork.f
#
Passes.obj: Passes.f
	ifort  $(FLAGS)     /c  Passes.f
#
ActivateFunctions.obj: ActivateFunctions.f
	ifort  $(FLAGS)     /c  ActivateFunctions.f
