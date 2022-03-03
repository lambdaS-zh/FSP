---- Fceux SubSystem for Prediction ----
---- Transplanted from Project-SimpleNES ----

local INS_MODE_MASK = 0x3       -- InstructionModeMask

local OPR_MASK = 0xE0           -- OperationMask
local OPR_SHF = 5               -- OperationShift

local ADDR_MODE_MASK = 0x1C     -- AddrModeMask
local ADDR_MODE_SHF = 2         -- AddrModeShift

local BRCH_INS_MASK = 0x1F      -- BranchInstructionMask
local BRCH_INS_MASK_RES = 0x10  -- BranchInstructionMaskResult
local BRCH_COND_MASK = 0x20     -- BranchConditionMask
local BRCH_ON_FLAG_SHF = 6      -- BranchOnFlagShift

local NMI_VECTOR = 0xFFFA
local RESET_VECTOR = 0xFFFC
local IRQ_VECTOR = 0xFFFE

local BRCH_ON_FLAG = {
    NEGATIVE =  0,
    OVERFLOW =  1,
    CARRY =     2,
    ZERO =      3,
}

-- OPCODE1
local OPC1 = {
    ORA = 0,
    AND = 1,
    EOR = 2,
    ADC = 3,
    STA = 4,
    LDA = 5,
    CMP = 6,
    SBC = 7,
}

local ADDR_MODE1 = {
    INDEXED_INDIRECT_X =0,
    ZERO_PAGE =         1,
    IMMEDIATE =         2,
    ABSOLUTE =          3,
    INDIRECT_Y =        4,
    INDEXED_X =         5,
    ABSOLUTE_Y =        6,
    ABSOLUTE_X =        7,
}

--OPCODE2
local OPC2 = {
    ASL = 0,
    ROL = 1,
    LSR = 2,
    ROR = 3,
    STX = 4,
    LDX = 5,
    DEC = 6,
    INC = 7,
}

local ADDR_MODE2 = {
    IMMEDIATE_ =        0,
    ZERO_PAGE_ =        1,
    ACCUMULATOR =       2,
    ABSOLUTE_ =         3,
    INDEXED =           5,
    ABSOLUTE_INDEXED =  7,
}

--OPCODE0
local OPC0 = {
    BIT = 1,
    STY = 4,
    LDY = 5,
    CPY = 6,
    CPX = 7,
}

-- OPCODE(Implied)
local OPCI = {
    NOP = 0xea,
    BRK = 0x00,
    JSR = 0x20,
    RTI = 0x40,
    RTS = 0x60,

    JMP  = 0x4C,
    JMPI = 0x6C, --JMP Indirect

    PHP = 0x08,
    PLP = 0x28,
    PHA = 0x48,
    PLA = 0x68,

    DEY = 0x88,
    DEX = 0xca,
    TAY = 0xa8,
    INY = 0xc8,
    INX = 0xe8,

    CLC = 0x18,
    SEC = 0x38,
    CLI = 0x58,
    SEI = 0x78,
    TYA = 0x98,
    CLV = 0xb8,
    CLD = 0xd8,
    SED = 0xf8,

    TXA = 0x8a,
    TXS = 0x9a,
    TAX = 0xaa,
    TSX = 0xba,
}

local INTR = {
    IRQ = 0,
    NMI = 1,
    BRK_ = 2,
}

local function u8(value)
    while value < 0 do
        value = value + 0x100
    end
    return AND(value, 0xFF)
end

local function i8(value)
    while value < -128 do
        value = value + 0x100
    end
    while value > 127 do
        value = value - 0x100
    end
    return value
end

local function u16(value)
    while value < 0 do
        value = value + 0x10000
    end
    return AND(value, 0xFFFF)
end

local function b2i(value)
    if value then
        return 1
    else
        return 0
    end
end

local function i2b(value)
    return (value ~= 0)
end

local rshift_switch = {
    [0] = function(value) return value end,
    [1] = function(value) return (value / 2) end,
    [2] = function(value) return (value / 4) end,
    [3] = function(value) return (value / 8) end,
    [4] = function(value) return (value / 16) end,
    [5] = function(value) return (value / 32) end,
    [6] = function(value) return (value / 64) end,
    [7] = function(value) return (value / 128) end,
    [8] = function(value) return (value / 256) end,
}
local function RSHIFT(value, steps)
    -- FCEUX bitwise func style: uppercase
    return rshift_switch[steps](value)
end

local lshift_switch = {
    [0] = function(value) return value end,
    [1] = function(value) return (value * 2) end,
    [2] = function(value) return (value * 4) end,
    [3] = function(value) return (value * 8) end,
    [4] = function(value) return (value * 16) end,
    [5] = function(value) return (value * 32) end,
    [6] = function(value) return (value * 64) end,
    [7] = function(value) return (value * 128) end,
    [8] = function(value) return (value * 256) end,
}
local function LSHIFT(value, steps)
    -- FCEUX bitwise func style: uppercase
    return lshift_switch[steps](value)
end

local function NOT_U8(value)
    -- NOTE: bitwise NOT (not logic NOT)
    return XOR(value, 0xFF)
end

local function NOT_U16(value)
    -- NOTE: bitwise NOT (not logic NOT)
    return XOR(value, 0xFFFF)
end

local function xor(l, r)
    -- NOTE: logic xor (not bitwise xor)
    -- builtin logical operator style: lowercase
    -- l and r must be of boolean type
    return l ~= r
end

local getr = function(name) return memory.getregister(name) end

local function getf_N()
    local p = getr("p")
    return (AND(p, BIT(7)) ~= 0)
end

local function getf_V()
    local p = getr("p")
    return (AND(p, BIT(6)) ~= 0)
end

local function getf_U()
    local p = getr("p")
    return (AND(p, BIT(5)) ~= 0)
end

local function getf_B()
    local p = getr("p")
    return (AND(p, BIT(4)) ~= 0)
end

local function getf_D()
    local p = getr("p")
    return (AND(p, BIT(3)) ~= 0)
end

local function getf_I()
    local p = getr("p")
    return (AND(p, BIT(2)) ~= 0)
end

local function getf_Z()
    local p = getr("p")
    return (AND(p, BIT(1)) ~= 0)
end

local function getf_C()
    local p = getr("p")
    return (AND(p, BIT(0)) ~= 0)
end

local function mainbus_new()
    local mainbus = {
        parent =            nil,
        top_layer =         {},
    }

    function mainbus:read(address)
        local value = self.top_layer[address]
        if value ~= nil then return value end
        if self.parent ~= nil then return self.parent:read(address) end
        return memory.readbyteunsigned(address)
    end

    function mainbus:write(address, value)
        self.top_layer[address] = value
    end

    function mainbus:fork()
        local child = mainbus_new()
        child.parent = self
        return child
    end

    return mainbus
end

local function cpu_new(mainbus)
    local cpu = {
        bus =               mainbus,

        cycles =            debugger.getcyclescount(),
        skip_cycles =       0,

        -- registers
        r_PC =              getr("pc"),
        r_SP =              getr("s"),
        r_A =               getr("a"),
        r_X =               getr("x"),
        r_Y =               getr("y"),

        -- status flags (f_B not used)
        f_C =               getf_C(),
        f_Z =               getf_Z(),
        f_I =               getf_I(),
        f_D =               getf_D(),
        f_V =               getf_V(),
        f_N =               getf_N(),
    }

    function cpu:fork()
        local child_bus = self.bus:fork()
        local child_cpu = cpu_new(child_bus)

        child_cpu.cycles = self.cycles
        child_cpu.skip_cycles = self.skip_cycles

        child_cpu.r_PC = self.r_PC
        child_cpu.r_SP = self.r_SP
        child_cpu.r_A = self.r_A
        child_cpu.r_X = self.r_X
        child_cpu.r_Y = self.r_Y

        child_cpu.f_C = self.f_C
        child_cpu.f_Z = self.f_Z
        child_cpu.f_I = self.f_I
        child_cpu.f_D = self.f_D
        child_cpu.f_V = self.f_V
        child_cpu.f_N = self.f_N

        return child_cpu
    end

    function cpu:interrupt(intr)
        if self.f_I and (intr ~= INTR.NMI) and (intr ~= INTR.BRK_) then
            return
        end

        if intr == INTR.BRK_ then
            self:setr_PC(self.r_PC + 1)
        end

        self:push_stack(RSHIFT(self.r_PC, 8))
        self:push_stack(self.r_PC)

        local flags = OR(
            LSHIFT(b2i(self.f_N), 7),
            LSHIFT(b2i(self.f_V), 6),
            LSHIFT(1, 5),  -- unused bit
            LSHIFT(b2i(intr == INTR.BRK_), 4),
            LSHIFT(b2i(self.f_D), 3),
            LSHIFT(b2i(self.f_I), 2),
            LSHIFT(b2i(self.f_Z), 1),
            b2i(self.f_C)
        )
        self:push_stack(flags)

        self.f_I = true

        if (intr == INTR.IRQ) or (intr == INTR.BRK_) then
            self:setr_PC(self:read_address_u16(IRQ_VECTOR))
        elseif intr == INTR.NMI then
            self:setr_PC(self:read_address_u16(NMI_VECTOR))
        end

        self.skip_cycles = self.skip_cycles + 7
    end

    function cpu:push_stack(value)
        self.bus:write(OR(0x100, self.r_SP), value)
        self:setr_SP(self.r_SP - 1)
    end

    function cpu:pull_stack()
        self:setr_SP(self.r_SP + 1)
        return self.bus:read(OR(0x100, self.r_SP))
    end

    function cpu:set_ZN(value)
        self.f_Z = not i2b(value)
        self.f_N = i2b(AND(value, 0x80))
    end

    function cpu:set_page_crossed(a, b, inc)
        if AND(a, 0xFF00) ~= AND(b, 0xFF00) then
            self.skip_cycles = self.skip_cycles + inc
        end
    end

    function cpu:skip_DMA_cycles()
        self.skip_cycles = self.skip_cycles + 513 + AND(self.cycles, 1)
    end

    function cpu:step()
        self.cycles = self.cycles + 1

        -- <NOT USED>'skip_cycles'(from SimpleNES).
        self.skip_cycles = 0

        local opcode = self.bus:read(self.r_PC)
        self:setr_PC(self.r_PC + 1)

        if self:execute_implied(opcode) then return end
        if self:execute_branch(opcode) then return end
        if self:execute_type1(opcode) then return end
        if self:execute_type2(opcode) then return end
        if self:execute_type0(opcode) then return end
        -- TODO: LOG
    end

    local implied_switch = {
        [OPCI.NOP] =    function(self) end,
        [OPCI.BRK] =    function(self)
            self:interrupt(INTR.BRK_)
        end,
        [OPCI.JSR] =    function(self)
            self:push_stack(u8((self.r_PC + 1) / 256))  -- >> 8
            self:push_stack(u8(self.r_PC + 1))
            self:setr_PC(self:read_address_u16(self.r_PC))
        end,
        [OPCI.RTS] =    function(self)
            self:setr_PC(self:pull_stack())
            self:setr_PC(OR(self.r_PC, self:pull_stack() * 256) + 1)  -- << 8
        end,
        [OPCI.RTI] =    function(self)
            local flags = self:pull_stack()
            self.f_N = i2b(AND(flags, 0x80))
            self.f_V = i2b(AND(flags, 0x40))
            self.f_D = i2b(AND(flags, 0x08))
            self.f_I = i2b(AND(flags, 0x04))
            self.f_Z = i2b(AND(flags, 0x02))
            self.f_C = i2b(AND(flags, 0x01))
            self:setr_PC(self:pull_stack())
            self:setr_PC(OR(self.r_PC, self:pull_stack() * 256))  -- << 8
        end,
        [OPCI.JMP] =    function(self)
            self:setr_PC(self:read_address_u16(self.r_PC))
        end,
        [OPCI.JMPI] =   function(self)
            local location = self:read_address_u16(self.r_PC)
            local page = AND(location, 0xFF00)
            self:setr_PC(OR(
                self.bus:read(location),
                256 * self.bus:read(
                    OR(
                        page,
                        AND(location + 1, 0xFF)
                    )
                )
            ))
        end,
        [OPCI.PHP] =    function(self)
            local flags = 
                b2i(self.f_N) * 128 +
                b2i(self.f_V) * 64 +
                1 * 32 +  -- supposed to always be 1
                1 * 16 +  -- PHP pushes with the B flag as 1, no matter what
                b2i(self.f_D) * 8 +
                b2i(self.f_I) * 4 +
                b2i(self.f_Z) * 2 +
                b2i(self.f_C)
            self:push_stack(flags)
        end,
        [OPCI.PLP] =    function(self)
            local flags = self:pull_stack()
            self.f_N = i2b(AND(flags, 0x80))
            self.f_V = i2b(AND(flags, 0x40))
            self.f_D = i2b(AND(flags, 0x08))
            self.f_I = i2b(AND(flags, 0x04))
            self.f_Z = i2b(AND(flags, 0x02))
            self.f_C = i2b(AND(flags, 0x01))
        end,
        [OPCI.PHA] =    function(self)
            self:push_stack(self.r_A)
        end,
        [OPCI.PLA] =    function(self)
            self:setr_A(self:pull_stack())
            self:set_ZN(self.r_A)
        end,
        [OPCI.DEY] =    function(self)
            self:setr_Y(self.r_Y - 1)
            self:set_ZN(self.r_Y)
        end,
        [OPCI.DEX] =    function(self)
            self:setr_X(self.r_X - 1)
            self:set_ZN(self.r_X)
        end,
        [OPCI.TAY] =    function(self)
            self:setr_Y(self.r_A)
            self:set_ZN(self.r_Y)
        end,
        [OPCI.INY] =    function(self)
            self:setr_Y(self.r_Y + 1)
            self:set_ZN(self.r_Y)
        end,
        [OPCI.INX] =    function(self)
            self:setr_X(self.r_X + 1)
            self:set_ZN(self.r_X)
        end,
        [OPCI.CLC] =    function(self)
            self.f_C = false
        end,
        [OPCI.SEC] =    function(self)
            self.f_C = true
        end,
        [OPCI.CLI] =    function(self)
            self.f_I = false
        end,
        [OPCI.SEI] =    function(self)
            self.f_I = true
        end,
        [OPCI.CLD] =    function(self)
            self.f_D = false
        end,
        [OPCI.SED] =    function(self)
            self.f_D = true
        end,
        [OPCI.TYA] =    function(self)
            self:setr_A(self.r_Y)
            self:set_ZN(self.r_A)
        end,
        [OPCI.CLV] =    function(self)
            self.f_V = false
        end,
        [OPCI.TXA] =    function(self)
            self:setr_A(self.r_X)
            self:set_ZN(self.r_A)
        end,
        [OPCI.TXS] =    function(self)
            self:setr_SP(self.r_X)
        end,
        [OPCI.TAX] =    function(self)
            self:setr_X(self.r_A)
            self:set_ZN(self.r_X)
        end,
        [OPCI.TSX] =    function(self)
            self:setr_X(self.r_SP)
            self:set_ZN(self.r_X)
        end,
    }
    function cpu:execute_implied(opcode)
        local f = implied_switch[opcode]
        if f == nil then
            return false
        end
        f(self)
        return true
    end

    local branch_switch = {
        [BRCH_ON_FLAG.NEGATIVE] =   'f_N',
        [BRCH_ON_FLAG.OVERFLOW] =   'f_V',
        [BRCH_ON_FLAG.CARRY] =      'f_C',
        [BRCH_ON_FLAG.ZERO] =       'f_Z',
    }
    function cpu:execute_branch(opcode)
        if AND(opcode, BRCH_INS_MASK) ~= BRCH_INS_MASK_RES then
            return false
        end

        local branch = i2b(AND(opcode, BRCH_COND_MASK))
        local reg_name = branch_switch[RSHIFT(opcode, BRCH_ON_FLAG_SHF)]
        if reg_name == nil then
            return false
        end

        -- seems like EQUAL TO 'branch == self[reg_name]'
        branch = not xor(branch, self[reg_name])
        if not branch then
            self:setr_PC(self.r_PC + 1)
        else
            local offset = i8(self.bus:read(self.r_PC))
            self:setr_PC(self.r_PC + 1)

            local new_pc = u16(self.r_PC + offset)
            self:set_page_crossed(self.r_PC, new_pc, 2)
            self:setr_PC(new_pc)
        end

        return true
    end

    local type1_addr_switch = {
        [ADDR_MODE1.INDEXED_INDIRECT_X] =   function(self, op)
            local zero_addr = u8(self.r_X + self.bus:read(self.r_PC))
            self:setr_PC(self.r_PC + 1)

            local location = OR(
                self.bus:read(AND(zero_addr, 0xFF)),
                self.bus:read(AND(zero_addr + 1, 0xFF)) * 256
            )
            return u16(location)
        end,
        [ADDR_MODE1.ZERO_PAGE] =            function(self, op)
            local location = self.bus:read(self.r_PC)
            self:setr_PC(self.r_PC + 1)
            return location
        end,
        [ADDR_MODE1.IMMEDIATE] =            function(self, op)
            local location = self.r_PC
            self:setr_PC(self.r_PC + 1)
            return location
        end,
        [ADDR_MODE1.ABSOLUTE] =             function(self, op)
            local location = self:read_address_u16(self.r_PC)
            self:setr_PC(self.r_PC + 2)
            return location
        end,
        [ADDR_MODE1.INDIRECT_Y] =           function(self, op)
            local zero_addr = self.bus:read(self.r_PC)
            self:setr_PC(self.r_PC + 1)
            local location = OR(
                self.bus:read(AND(zero_addr, 0xFF)),
                LSHIFT(self.bus:read(AND(zero_addr + 1, 0xFF)), 8)
            )
            if op ~= OPC1.STA then
                self:set_page_crossed(location, location + self.r_Y)
            end
            return location + self.r_Y
        end,
        [ADDR_MODE1.INDEXED_X] =            function(self, op)
            local location = AND(
                self.bus:read(self.r_PC) + self.r_X,
                0xFF
            )
            self:setr_PC(self.r_PC + 1)
            return location
        end,
        [ADDR_MODE1.ABSOLUTE_Y] =           function(self, op)
            local location = self:read_address_u16(self.r_PC)
            self:setr_PC(self.r_PC + 2)
            if op ~= OPC1.STA then
                self:set_page_crossed(location, location + self.r_Y)
            end
            return location + self.r_Y
        end,
        [ADDR_MODE1.ABSOLUTE_X] =           function(self, op)
            local location = self:read_address_u16(self.r_PC)
            self:setr_PC(self.r_PC + 2)
            if op ~= OPC1.STA then
                self:set_page_crossed(location, location + self.r_X)
            end
            return location + self.r_X
        end,
    }
    local op1_switch = {
        [OPC1.ORA] =                        function(self, location)
            self:setr_A(OR(self.r_A, self.bus:read(location)))
            self:set_ZN(self.r_A)
        end,
        [OPC1.AND] =                        function(self, location)
            self:setr_A(AND(self.r_A, self.bus:read(location)))
            self:set_ZN(self.r_A)
        end,
        [OPC1.EOR] =                        function(self, location)
            self:setr_A(XOR(self.r_A, self.bus:read(location)))
            self:set_ZN(self.r_A)
        end,
        [OPC1.ADC] =                        function(self, location)
            local operand = self.bus:read(location)
            local sum = self.r_A + operand + b2i(self.f_C)
            self.f_C = i2b(AND(sum, 0x100))
            self.f_V = i2b(AND(
                XOR(self.r_A, sum),
                XOR(operand, sum),
                0x80
            ))
            self:setr_A(sum)
            self:set_ZN(self.r_A)
        end,
        [OPC1.STA] =                        function(self, location)
            self.bus:write(location, self.r_A)
        end,
        [OPC1.LDA] =                        function(self, location)
            self:setr_A(self.bus:read(location))
            self:set_ZN(self.r_A)
        end,
        [OPC1.SBC] =                        function(self, location)
            local subtrahend = u16(self.bus:read(location))
            local diff = u16(self.r_A - subtrahend - b2i(not self.f_C))
            self.f_C = not i2b(AND(diff, 0x100))
            self.f_V = i2b(AND(
                XOR(self.r_A, diff),
                XOR(NOT_U16(subtrahend), diff),
                0x80
            ))
            self:setr_A(diff)
            self:set_ZN(diff)
        end,
        [OPC1.CMP] =                        function(self, location)
            local diff = u16(self.r_A - self.bus:read(location))
            self.f_C = not i2b(AND(diff, 0x100))
            self:set_ZN(diff)
        end,
    }
    function cpu:execute_type1(opcode)
        if AND(opcode, INS_MODE_MASK) ~= 0x1 then
            return false
        end

        local op = RSHIFT(AND(opcode, OPR_MASK), OPR_SHF)
        local mode = RSHIFT(AND(opcode, ADDR_MODE_MASK), ADDR_MODE_SHF)
        local location = 0
    
        local f = type1_addr_switch[mode]
        if f == nil then
            return false
        end
        location = u16(f(self, op))

        f = op1_switch[op]
        if f == nil then
            return false
        end
        f(self, location)

        return true
    end

    local type2_addr_switch = {
        [ADDR_MODE2.IMMEDIATE_] =           function(self, op)
            local location = self.r_PC
            self:setr_PC(self.r_PC + 1)
            return location
        end,
        [ADDR_MODE2.ZERO_PAGE_] =           function(self, op)
            local location = self.bus:read(self.r_PC)
            self:setr_PC(self.r_PC + 1)
            return location
        end,
        [ADDR_MODE2.ACCUMULATOR] =          function(self, op)
            return 0
        end,
        [ADDR_MODE2.ABSOLUTE_] =            function(self, op)
            local location = self:read_address_u16(self.r_PC)
            self:setr_PC(self.r_PC + 2)
            return location
        end,
        [ADDR_MODE2.INDEXED] =              function(self, op)
            local location = self.bus:read(self.r_PC)
            self:setr_PC(self.r_PC + 1)
            local index = 0
            if (op == OPC2.LDX) or (op == OPC2.STX) then
                index = self.r_Y
            else
                index = self.r_X
            end
            return AND(0xFF, location + index)
        end,
        [ADDR_MODE2.ABSOLUTE_INDEXED] =     function(self, op)
            local location = self:read_address_u16(self.r_PC)
            self:setr_PC(self.r_PC + 2)
            local index = 0
            if (op == OPC2.LDX) or (op == OPC2.STX) then
                index = self.r_Y
            else
                index = self.r_X
            end
            self:set_page_crossed(location, location + index)
            return location + index
        end,
    }
    local op2_switch = {
        [OPC2.ASL] =                        function(self, op, location, mode)
            if mode == ADDR_MODE2.ACCUMULATOR then
                local prev_c = self.f_C
                self.f_C = i2b(AND(self.r_A, 0x80))
                self:setr_A(LSHIFT(self.r_A, 1))
                if prev_c and (op == OPC2.ROL) then
                    self:setr_A(OR(self.r_A, 1))
                end
                self:set_ZN(self.r_A)
            else
                local prev_c = self.f_C
                local operand = self.bus:read(location)
                self.f_C = i2b(AND(operand, 0x80))
                operand = u16(LSHIFT(operand, 1))
                if prev_c and (op == OPC2.ROL) then
                    operand = OR(operand, 1)
                end
                self:set_ZN(operand)
                self.bus:write(location, operand)
            end
        end,
        [OPC2.LSR] =                        function(self, op, location, mode)
            if mode == ADDR_MODE2.ACCUMULATOR then
                local prev_c = self.f_C
                self.f_C = i2b(AND(self.r_A, 1))
                self:setr_A(RSHIFT(self.r_A, 1))
                if prev_c and (op == OPC2.ROR) then
                    self:setr_A(OR(self.r_A, 0x80))  -- 0b10000000
                end
                self:set_ZN(self.r_A)
            else
                local prev_c = self.f_C
                local operand = self.bus:read(location)
                self.f_C = i2b(AND(operand, 1))
                operand = RSHIFT(operand, 1)
                if prev_c and (op == OPC2.ROR) then
                    operand = OR(operand, 0x80) -- 0b10000000
                end
                self:set_ZN(operand)
                self.bus:write(location, operand)
            end
        end,
        [OPC2.STX] =                        function(self, op, location, mode)
            self.bus:write(location, self.r_X)
        end,
        [OPC2.LDX] =                        function(self, op, location, mode)
            self:setr_X(self.bus:read(location))
            self:set_ZN(self.r_X)
        end,
        [OPC2.DEC] =                        function(self, op, location, mode)
            local tmp = self.bus:read(location) - 1
            self:set_ZN(tmp)
            self.bus:write(location, tmp)
        end,
        [OPC2.INC] =                        function(self, op, location, mode)
            local tmp = self.bus:read(location) + 1
            self:set_ZN(tmp)
            self.bus:write(location, tmp)
        end,
    }
    op2_switch[OPC2.ROL] = op2_switch[OPC2.ASL]
    op2_switch[OPC2.ROR] = op2_switch[OPC2.LSR]
    function cpu:execute_type2(opcode)
        if AND(opcode, INS_MODE_MASK) ~= 0x2 then
            return false
        end

        local op = RSHIFT(AND(opcode, OPR_MASK), OPR_SHF)
        local mode = RSHIFT(AND(opcode, ADDR_MODE_MASK), ADDR_MODE_SHF)
        local location = 0

        local f = type2_addr_switch[mode]
        if f == nil then
            return false
        end
        location = u16(f(self, op))

        f = op2_switch[op]
        if f == nil then
            return false
        end
        f(self, op, location, mode)

        return true
    end

    local type0_addr_switch = {
        [ADDR_MODE2.IMMEDIATE_] =           function(self, op) -- TODO: del args
            local location = self.r_PC
            self:setr_PC(self.r_PC + 1)
            return location
        end,
        [ADDR_MODE2.ZERO_PAGE_] =           function(self, op)
            local location = self.bus:read(self.r_PC)
            self:setr_PC(self.r_PC + 1)
            return location
        end,
        [ADDR_MODE2.ABSOLUTE_] =            function(self, op)
            local location = self:read_address_u16(self.r_PC)
            self:setr_PC(self.r_PC + 2)
            return location
        end,
        [ADDR_MODE2.INDEXED] =              function(self, op)
            local location = AND(0xFF, self.bus:read(self.r_PC) + self.r_X)
            self:setr_PC(self.r_PC + 1)
            return location
        end,
        [ADDR_MODE2.ABSOLUTE_INDEXED] =     function(self, op)
            local location = self:read_address_u16(self.r_PC)
            self:setr_PC(self.r_PC + 2)
            self:set_page_crossed(location, location + self.r_X)
            return location + self.r_X
        end,
    }
    local op0_switch = {
        [OPC0.BIT] =                        function(self, location)
            local operand = self.bus:read(location)
            self.f_Z = not i2b(AND(self.r_A, operand))
            sefl.f_V = i2b(AND(operand, 0x40))
            sefl.f_N = i2b(AND(operand, 0x80))
        end,
        [OPC0.STY] =                        function(self, location)
            self.bus:write(location, self.r_Y)
        end,
        [OPC0.LDY] =                        function(self, location)
            self:setr_Y(self.bus:read(location))
            self:set_ZN(self.r_Y)
        end,
        [OPC0.CPY] =                        function(self, location)
            local diff = u16(self.r_Y - self.bus:read(location))
            self.f_C = not i2b(AND(diff, 0x100))
            self:set_ZN(diff)
        end,
        [OPC0.CPX] =                        function(self, location)
            local diff = u16(self.r_X - self.bus:read(location))
            self.f_C = not i2b(AND(diff, 0x100))
            self:set_ZN(diff)
        end,
    }
    function cpu:execute_type0(opcode)
        if AND(opcode, INS_MODE_MASK) ~= 0x0 then
            return false
        end

        local op = RSHIFT(AND(opcode, OPR_MASK), OPR_SHF)
        local mode = RSHIFT(AND(opcode, ADDR_MODE_MASK), ADDR_MODE_SHF)
        local location = 0

        local f = type0_addr_switch[mode]
        if f == nil then
            return false
        end
        location = u16(f(self, op))

        f = op0_switch[op]
        if f == nil then
            return false
        end
        f(self, location)

        return true
    end

    function cpu:read_address_u16(addr)
        return OR(
            self.bus:read(addr),
            LSHIFT(self.bus:read(addr + 1), 8)
        )
    end

    function cpu:setr_PC(value)
        self.r_PC = u16(value)
    end

    function cpu:setr_SP(value)
        self.r_SP = u8(value)
    end

    function cpu:setr_A(value)
        self.r_A = u8(value)
    end

    function cpu:setr_X(value)
        self.r_X = u8(value)
    end

    function cpu:setr_Y(value)
        self.r_Y = u8(value)
    end

    return cpu
end

local s_last_test_result = {}

local function throw()
    emu.print("------real------")
    emu.print(string.format(
        "PC=%X, SP=%X, A=%X, X=%X, Y=%X",
        getr("pc"),
        getr("s"),
        getr("a"),
        getr("x"),
        getr("y")
    ))
    emu.print(string.format(
        "C=%s, Z=%s, I=%s, D=%s, V=%s, N=%s",
        tostring(getf_C()),
        tostring(getf_Z()),
        tostring(getf_I()),
        tostring(getf_D()),
        tostring(getf_V()),
        tostring(getf_N())
    ))

    emu.print("------fake------")
    emu.print(string.format(
        "PC=%X, SP=%X, A=%X, X=%X, Y=%X",
        s_last_test_result.r_PC,
        s_last_test_result.r_SP,
        s_last_test_result.r_A,
        s_last_test_result.r_X,
        s_last_test_result.r_Y
    ))
    emu.print(string.format(
        "C=%s, Z=%s, I=%s, D=%s, V=%s, N=%s",
        tostring(s_last_test_result.f_C),
        tostring(s_last_test_result.f_Z),
        tostring(s_last_test_result.f_I),
        tostring(s_last_test_result.f_D),
        tostring(s_last_test_result.f_V),
        tostring(s_last_test_result.f_N)
    ))

    debugger.hitbreakpoint()
end

local function _exec_cb()
    -- this function will be called before each real instruction.
    if s_last_test_result.started ~= nil then
        --if s_last_test_result.r_PC ~= getr("pc") then throw() end
        --if s_last_test_result.r_SP ~= getr("s") then throw() end
        if s_last_test_result.r_A ~= getr("a") then throw() end
        if s_last_test_result.r_X ~= getr("x") then throw() end
        if s_last_test_result.r_Y ~= getr("y") then throw() end
        if s_last_test_result.f_C ~= getf_C() then throw() end
        if s_last_test_result.f_Z ~= getf_Z() then throw() end
        if s_last_test_result.f_I ~= getf_I() then throw() end
        if s_last_test_result.f_D ~= getf_D() then throw() end
        if s_last_test_result.f_V ~= getf_V() then throw() end
        if s_last_test_result.f_N ~= getf_N() then throw() end
    end
    s_last_test_result.started = true

    local bus = mainbus_new()
    local cpu = cpu_new(bus)
    cpu:step() -- emulate executing one instruction
    s_last_test_result.r_PC = cpu.r_PC
    s_last_test_result.r_SP = cpu.r_SP
    s_last_test_result.r_A = cpu.r_A
    s_last_test_result.r_X = cpu.r_X
    s_last_test_result.r_Y = cpu.r_Y
    s_last_test_result.f_C = cpu.f_C
    s_last_test_result.f_Z = cpu.f_Z
    s_last_test_result.f_I = cpu.f_I
    s_last_test_result.f_D = cpu.f_D
    s_last_test_result.f_V = cpu.f_V
    s_last_test_result.f_N = cpu.f_N
end

local function self_test_loop()
    memory.registerexec(0x8000, 0x10000 - 0x8000, _exec_cb)
    while true do
        emu.frameadvance()
    end
end

-- Uncomment the code below to execute self-test.
--self_test_loop()
