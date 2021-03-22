# optional params
DEBUG ?= no

# standard params
CXX = clang++
INCLUDE = -Iengine/include -Iinclude/component_system -Iinclude/resource_management -Iinclude/utils -I/usr/include/SDL2/
CXXFLAGS = -std=c++17 $(INCLUDE)

LDFLAGS := 
LDLIBS := -lSDL2 -ldl -lSDL2_ttf -lSDL2_mixer -lSDL2_image

# optional params checks
ifneq ($(DEBUG), no)
	CXXFLAGS += -g
endif

# src directories
MAIN_SRC_DIR := engine/src

MAIN_OBJ_DIR := obj

BIN_DIR := lib

EXE = $(BIN_DIR)/libengine.so

# src file list
MAIN_SRC = engine.cpp engine_capi.cpp

MAIN_DPD = $(MAIN_SRC:%.cpp=$(MAIN_OBJ_DIR)/%.dpd)
# set up object file list
MAIN_OBJ = $(MAIN_SRC:%.cpp=$(MAIN_OBJ_DIR)/%.o)

.PHONY: all clean

all: $(EXE)

# linking
$(EXE): $(MAIN_OBJ) | $(BIN_DIR)
	$(CXX) $(LDFLAGS) $^ $(LDLIBS) -shared -o $@

# make dir if not exist
$(BIN_DIR) $(MAIN_OBJ_DIR):
	mkdir -p $@

# object files
$(MAIN_OBJ_DIR)/%.o : $(MAIN_SRC_DIR)/%.cpp | $(MAIN_OBJ_DIR)
	$(CXX) -fPIC -c $(<) -o $(@) $(CXXFLAGS)

# dpd files
$(MAIN_OBJ_DIR)/%.dpd : $(MAIN_SRC_DIR)/%.cpp | $(MAIN_OBJ_DIR)
	$(CXX) -MM $(<) -o $(@) $(CXXFLAGS) -MT $(<:.cpp=.o)

clean:
	@$(RM) -rv $(BIN_DIR) $(MAIN_OBJ_DIR)

-include $(MAIN_DPD)