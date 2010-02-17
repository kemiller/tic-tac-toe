class BF:
    """brainfuck interpreter

    This is a selfcontained machine for running a brainfuck program.
    Programs can be run by calling run() or runStep().  A program
    finished when its program counter reaches the end of the program.
    Output can be retrieved from the output list.
    """
    def __init__(self,program,memsize=30000):
        """Constructs a program machine.

        Loads a program into the machine, initializes a cell array
        to 0.  The cell array can be specified in the 2nd parameter.
        The default is 30000.  memp and pc are set to 0.
        """
        self.program = program
        self.memory = [0 for i in xrange(memsize)]
        self.output = []
        self.memp = 0
        self.pc = 0

        # if I wanted to be really "good", I'd split all the
        # operations into their own operations classes that accept
        # a BF machine to do their operations on...
        #
        # 1st element in tuple is the function object
        # 2nd element is the pretty string format
        # 3rd element is whether or not the instruction "accumulates"
        #   That is, for multiple same instructions in a row, does the
        #   pretty string routine compress them.
        # 4th is the indentation adjustment.
        self.operations = {
            '>': (self.advance_memp, "memp += %d", True, 0),
            '<': (self.retreat_memp, "memp -= %d", True, 0),
            '+': (self.increment_memp, "memory[memp] += %d", True, 0),
            '-': (self.decrement_memp, "memory[memp] -= %d", True, 0),
            '.': (self.output_memp, "print memory[memp],", False, 0),
            ',': (self.input_memp, "input memory[memp]", False, 0),
            '[': (self.while_true, "while(memory[memp] != 0):", False, 1),
            ']': (self.end_while_true, "", False, -1)
            }

    def runStep(self):
        """Executes a program for exactly one instruction.
        """
        if (self.pc < len(self.program)):
            self.operations[self.program[self.pc]][0]()
            self.pc += 1

    def run(self):
        """Executes a program until self.finished() is true.
        """
        while not self.finished():
            self.runStep()        

    def finished(self):
        """Returns true when the program counter is past the end of the program.
        """
        return (self.pc >= len(self.program))

    def syntaxGood(self):
        """Checks for valid characters and balanced loop brackets.
        """
        bracket = 0
        for i in self.program:
            if i not in self.operations.keys():
                return false
            elif i == '[':
                bracket += 1
            elif i == ']':
                bracket -= 1
        return bracket == 0

    def pythonString(self):
        """Returns the brainfuck program as Python code.
        """
        snippets = []
        indent = 0
        char_accum = []

        snippets.append("memp = 0")
        snippets.append("memory = [0 for i in xrange(%d)]" % \
                        (len(self.memory)))
        for c in self.program:
            if c not in char_accum and char_accum:
                snippets.append("\t" * indent + \
                                self.operations[char_accum[0]][1] % \
                                (len(char_accum)))
                char_accum = []

            # does this operation accumulate?
            if self.operations[c][2]:
                char_accum.append(c)
            else:
                snippets.append("\t" * indent + self.operations[c][1])
                indent += self.operations[c][3]

        if char_accum:
            snippets.append("\t" * \
                            indent + self.operations[char_accum[0]][1] % \
                            (len(char_accum)))

        return "\n".join(snippets)

    def pythonCode(self):
        """Returns code compiled from the return value of pythonString()
        """
        return compile(self.pythonString(), "", "exec")

    def advance_memp(self):
        """Moves the memory pointer on step to the higher index.
        """
        self.memp += 1

    def retreat_memp(self):
        """Moves the memory pointer one step to the lower index.
        """
        self.memp -= 1
        
    def increment_memp(self):
        """Increments the memory currently pointed to by 1
        """
        self.memory[self.memp % len(self.memory)] += 1

    def decrement_memp(self):
        """Decrements the memory currently pointed to by 1
        """
        self.memory[self.memp % len(self.memory)] -= 1

    def output_memp(self):
        """Puts the value at the current memory location into the output list.
        """
        self.output.append(self.memory[self.memp])

    def input_memp(self):
        """Reads from the standard input.

        Only accepts integers (including negative numbers).  Error on
        input string.
        """
        try:
            self.memory[self.memp] = int(raw_input("bf>"))
        except EOFError, e:
            self.memory[self.memp] = 0
        except Exception,e:
            print str(e)

    def while_true(self):
        """Start of a loop.
        
        If the memory pointer points to 0, then advance to enclosing ']'
        """
        
        # seen_open keeps track of the
        # number of [ we have seen.  The loop only terminates
        # when we reach the corresponding ].  If seen_open isn't 0
        # then we know that we haven't hit the correct ]
        seen_open = 0
        if (self.memory[self.memp] == 0):
            while((self.program[self.pc] != ']' or seen_open > 0) and \
                  self.pc < len(self.program)):
                self.pc += 1
                if (self.program[self.pc] == '['):
                    seen_open += 1
                elif (self.program[self.pc] == ']' and seen_open > 0):
                    seen_open -= 1

    def end_while_true(self):
        """End of loop.

        Returns the program counter to the enclosing '['
        """
        seen_close = 0
        while ((self.program[self.pc] != '[' or seen_close > 0) and \
               self.pc >= 0):
            self.pc -= 1
            if (self.program[self.pc] == ']'):
                seen_close += 1
            elif (self.program[self.pc] == '[' and seen_close > 0):
                seen_close -= 1

        # because runStep will increment the program counter after
        # this method finishes, it needs to be offset by 1 so the
        # loop test will occur properly
        self.pc -= 1

