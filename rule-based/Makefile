GOAL="execute"
MAIN_FILE=main.pl
EXEC_FILE=languages.o
BUILD_DIR=build
SRC_DIR=src

dev:
	swipl -s $(SRC_DIR)/$(MAIN_FILE) -g $(GOAL)

$(BUILD_DIR): 
	mkdir $(BUILD_DIR)

compile: | $(BUILD_DIR)
	swipl --goal=$(GOAL) --stand_alone=true -o $(BUILD_DIR)/$(EXEC_FILE) -c $(SRC_DIR)/$(MAIN_FILE)

run: compile
	$(BUILD_DIR)/$(EXEC_FILE)