# optional params
DEBUG ?= no

# standard params
CXX = clang++
INCLUDE = -Iengine/include -Iinclude/component_system -Iinclude/resource_management -Iinclude/utils -I/usr/include/SDL2/
CXXFLAGS = -std=c++17 $(INCLUDE)
WINDOWS_CXX = x86_64-w64-mingw32-g++
WINDOWS_INCLUDE = -I/usr/x86_64-w64-mingw32/include -Iengine/include
WINDOWS_CXXFLAGS = -std=c++17 $(WINDOWS_INCLUDE)

LDFLAGS := 
LDLIBS := -lSDL2 -ldl -lSDL2_ttf -lSDL2_mixer -lSDL2_image

WINDOWS_LDFLAGS :=
WINDOWS_LDLIBS := -lmingw32 -lSDL2main -lSDL2

# optional params checks
ifneq ($(DEBUG), no)
	CXXFLAGS += -g
endif

# src directories
MAIN_SRC_DIR := engine/src

MAIN_OBJ_DIR := obj

BIN_DIR := engine-lib

LINUX = $(BIN_DIR)/libengine.so
WINDOWS = $(BIN_DIR)/libengine.dll

# src file list
MAIN_SRC = engine.cpp engine_capi.cpp

MAIN_DPD = $(MAIN_SRC:%.cpp=$(MAIN_OBJ_DIR)/%.dpd)
# set up object file list
MAIN_OBJ = $(MAIN_SRC:%.cpp=$(MAIN_OBJ_DIR)/%.o)
MAIN_OBJ_WINDOWS = $(MAIN_SRC:%.cpp=$(MAIN_OBJ_DIR)/%-windows.o)

.PHONY: all clean windows

all: $(LINUX) $(WINDOWS)

windows: $(WINDOWS)

# -std=c++17 engine/src/engine_capi.cpp engine/src/engine.cpp -I/usr/x86_64-w64-mingw32/include -Iengine/include 
# -lmingw32 -lSDL2main -lSDL2 -shared -o engine.dll

# linking
$(LINUX): $(MAIN_OBJ) | $(BIN_DIR)
	$(CXX) $(LDFLAGS) $^ $(LDLIBS) -shared -o $@

$(WINDOWS): $(MAIN_OBJ_WINDOWS) | $(BIN_DIR)
	$(WINDOWS_CXX) $(WINDOWS_LDFLAGS) $^ $(WINDOWS_LDLIBS) -shared -o $@

# make dir if not exist
$(BIN_DIR) $(MAIN_OBJ_DIR):
	mkdir -p $@

# object files
$(MAIN_OBJ_DIR)/%.o : $(MAIN_SRC_DIR)/%.cpp | $(MAIN_OBJ_DIR)
	$(CXX) -fPIC -c $(<) -o $(@) $(CXXFLAGS)

$(MAIN_OBJ_DIR)/%-windows.o : $(MAIN_SRC_DIR)/%.cpp | $(MAIN_OBJ_DIR)
	$(WINDOWS_CXX) -fPIC -c $(<) -o $(@) $(WINDOWS_CXXFLAGS)

# dpd files
$(MAIN_OBJ_DIR)/%.dpd : $(MAIN_SRC_DIR)/%.cpp | $(MAIN_OBJ_DIR)
	$(CXX) -MM $(<) -o $(@) $(CXXFLAGS) -MT $(<:.cpp=.o)

clean:
	@$(RM) -rv $(BIN_DIR) $(MAIN_OBJ_DIR)

-include $(MAIN_DPD)