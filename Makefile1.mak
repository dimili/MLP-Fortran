FLAGS = /O3
#FLAGS  = /O3          
#FLAGS  = /Zi
EXE     = TrainMBP
OBJ     = main.obj ReadWrite.obj TrainingModes.obj Passes.obj ActivateFunctions.obj

$(EXE): $(OBJ)
	ifort  $(FLAGS) $(OBJ) /o $(EXE)
#
main.obj: main.f
	ifort  $(FLAGS)     /c  main.f
#
ReadWrite.obj: ReadWrite.f
	ifort  $(FLAGS)     /c  ReadWrite.f
#
TrainingModes.obj: TrainingModes.f 
	ifort  $(FLAGS)     /c  TrainingModes.f
#
Passes.obj: Passes.f
	ifort  $(FLAGS)     /c  Passes.f
#
ActivateFunctions.obj: ActivateFunctions.f
	ifort  $(FLAGS)     /c  ActivateFunctions.f
