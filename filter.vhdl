-- Karan Aggarwal
-- 2019CS10699
-----------------------Sub Architectures------------------------------------------------
-- RAM
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity RAM_64Kx8 is port(
	clock : in std_logic;
	read_enable, write_enable : in std_logic;
	address : in std_logic_vector(15 downto 0);
	data_in : in std_logic_vector(7 downto 0);
	data_out : out std_logic_vector(7 downto 0)
);
end RAM_64Kx8;

architecture Artix of RAM_64Kx8 is
	type Memory_type is array (0 to 65535) of std_logic_vector (7 downto 0);
	signal Memory_array : Memory_type;
begin
process (clock) begin
	if rising_edge (clock) then
		if (read_enable = '1') then -- the data read is available after the clock edge
			data_out <= Memory_array (to_integer (unsigned (address)));
		end if;
		if (write_enable = '1') then
			Memory_array (to_integer (unsigned(address))) <= data_in;
		end if;
	end if;
end process;
end Artix;

-- ROM
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity ROM_32x9 is
port(
	clock : in std_logic;
	read_enable : in std_logic;
	address : in std_logic_vector(4 downto 0);
	data_out : out std_logic_vector(8 downto 0)
);
end ROM_32x9;

architecture Artix of ROM_32x9 is
	type Memory_type is array (0 to 31) of std_logic_vector (8 downto 0);
	signal Memory_array : Memory_type;
begin
process (clock) begin
	if rising_edge (clock) then
		if (read_enable = '1') then -- the data read is available after the clock edge
			data_out <= Memory_array (to_integer (unsigned (address)));
		end if;
	end if;
end process;
end Artix;

-- MAC
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity MAC is
port(
	clock : in std_logic;
	control : in std_logic;
	data_in1, data_in2 : in std_logic_vector(17 downto 0);
	data_out : out std_logic_vector(17 downto 0)
);
end MAC;

architecture Artix of MAC is
	signal sum, product : signed (17 downto 0);
begin
data_out <= std_logic_vector (sum);
product <= signed (data_in1) * signed (data_in2);
process (clock) begin
	if rising_edge (clock) then
		if (control = '0') then
			sum <= product;
		else
			sum <= product + sum;
		end if;
	end if;
end process;
end Artix;
-----------------End of Sub Architectures-----------------------------------------------

-----------------------Main Architecture------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

ENTITY filter IS
PORT(
    clk : in std_logic; -- Clock
    b1 : in std_logic; -- Button 1 (To start the filtering process)
    b2 : in std_logic); -- Button 2 (Switch)
	-- If this is set to '0' when Button 1 is pressed, smoothening will be done
	-- Else sharpening will be done
END filter;

ARCHITECTURE ARCH OF filter IS

TYPE state_type IS(S0, S1, S2, S3);
-- S0 - Initial state
-- S1 - A transition is made to this state when Button b1 is pressed 
-- (Also corresponds to the left and top most element (of the 9 elements currently being filtered) for each pixel of the filtered image.
-- S2 - This handles the rest of the 8 elements of the filtering process for each pixel of the filtered image.
-- S3 - The result for each pixel of the output is written to the corresponding address in memory here.
SIGNAL state : state_type := S0;

SIGNAL I : unsigned (7 DOWNTO 0) := "00000000";
-- Corrseponds to the same I given in the problem statement
SIGNAL J : unsigned (7 DOWNTO 0) := "00000000";
-- Corrseponds to the same J given in the problem statement
SIGNAL a : unsigned (1 DOWNTO 0) := "00";
-- Corrseponds to the i (small i) given in the problem statement
SIGNAL b : unsigned (1 DOWNTO 0) := "00";
-- Corrseponds to the j (small j) given in the problem statement
SIGNAL curr_address : unsigned(15 downto 0) := "0000000000000000";
-- I and J uniquely define an address in the ram given by 160*I + J;

SIGNAL ram_read_address : unsigned(15 downto 0) := "0000000000000000";
-- I, J, a, b determine the address of a pixel to be read from the RAM while filtering

SIGNAL ram_write_address : unsigned(15 downto 0) := "0000000000000000";
-- Address where write operation is done

SIGNAL ram_address : std_logic_vector(15 downto 0) := "0000000000000000";
-- Selects either ram_read_address or ram_write_address depending on state

SIGNAL ram_in : std_logic_vector(7 downto 0); -- Data to be written to the RAM
SIGNAL ram_out : std_logic_vector(7 downto 0); -- Data to be read from RAM
SIGNAL ram_read : std_logic := '0'; -- Control signal for RAM read operation
SIGNAL ram_write : std_logic := '0'; -- Control signal for RAM write operation

SIGNAL rom_address : unsigned(4 downto 0) := "00000";
-- Address to be read from ROM for getting co-efficients (determined by a and b)
SIGNAL final_rom_address : std_logic_vector(4 downto 0) := "00000";
-- A duplicate signal as ROM input PORT is std_logic_vector but unsigned is needed
-- for addition/subtraction

SIGNAL rom_out : std_logic_vector(8 downto 0); -- Data to be read from ROM
SIGNAL rom_read : std_logic := '0'; -- Control signal for ROM read operation

SIGNAL mac_in1 : std_logic_vector(17 downto 0); -- data_in1 for MAC
SIGNAL mac_in2 : std_logic_vector(17 downto 0); -- data_in2 for MAC
SIGNAL mac_out : std_logic_vector(17 downto 0); -- Result of the multiplication and additions
SIGNAL mac_control : std_logic := '1'; -- MAC control signal

SIGNAL c_ram_read_address : std_logic_vector(2 downto 0); -- Control signal for ram_read_address
SIGNAL c_curr_address : std_logic_vector(2 downto 0); -- Control signal for curr_address
SIGNAL c_rom_address : std_logic_vector(2 downto 0); -- Control signal for rom_address
SIGNAL c_ram_write_address : std_logic_vector(1 downto 0);-- Control signal for ram_write_address
SIGNAL c_I : std_logic_vector(1 downto 0); -- Control signal for I
SIGNAL c_J : std_logic_vector(1 downto 0); -- Control signal for J
SIGNAL c_a : std_logic_vector(1 downto 0); -- Control signal for a
SIGNAL c_b : std_logic_vector(1 downto 0); -- Control signal for b

-- Component declarations
COMPONENT RAM_64Kx8 IS
PORT(
    clock : in std_logic;
	read_enable, write_enable : in std_logic;
	address : in std_logic_vector(15 downto 0);
	data_in : in std_logic_vector(7 downto 0);
	data_out : out std_logic_vector(7 downto 0));
END COMPONENT RAM_64Kx8;

COMPONENT ROM_32x9 IS
PORT(
    clock : in std_logic;
	read_enable : in std_logic;
	address : in std_logic_vector(4 downto 0);
	data_out : out std_logic_vector(8 downto 0));
END COMPONENT ROM_32x9;

COMPONENT MAC IS
PORT(
    clock : in std_logic;
	control : in std_logic;
	data_in1, data_in2 : in std_logic_vector(17 downto 0);
	data_out : out std_logic_vector(17 downto 0));
END COMPONENT MAC;

BEGIN

-----------------State Transitions--------------------------------------------------------
PROCESS (clk) BEGIN
	IF clk'EVENT AND clk = '1' THEN
		IF state = S0 THEN
			IF b1 = '1' THEN state <= S1; -- If button 1 is pushed, start filtering
			END IF;
		ELSIF state = S1 THEN
			IF I = "01110111" THEN state <= S0; -- If I == 159 (Filtering is done), go to S0
			ELSE state <= S2; -- Go to S2 to process 8 remaining pixels corresponding to the pixel at (I, J)
			END IF;
		ELSIF state = S2 THEN
			IF a = "11" THEN state <= S3; -- If all 9 additions for a particular pixel are done, go to the write state
			END IF;
		ELSE state <= S1; -- In the write state, go to S1 to process the next pixel
		END IF;
	END IF;
END PROCESS;
-----------------End of State Transitions------------------------------------------------

-----------------Start of Control Part---------------------------------------------------
PROCESS (state, b1, b2) BEGIN
	IF state = S0 THEN
		ram_read <= '0'; ram_write <= '0'; c_ram_read_address <= "001"; c_curr_address <= "001";
		rom_read <= '0'; mac_control <= '0'; c_ram_write_address <= "01";
		c_I <= "01"; c_J <= "01"; c_a <= "01"; c_b <= "01";
		-- All control signals above are set to default values
		IF b1 = '1' AND b2 = '0' THEN
			c_rom_address <= "001";
			-- If b2 = '0', do smoothening (set rom_address to 0)
		ELSIF b1 = '1' AND b2 = '1' THEN
			c_rom_address <= "011";
			-- If b2 = '1', do sharpening (set rom_address to 9)
		END IF;
	ELSIF state = S1 THEN
		ram_write <= '0'; c_curr_address <= "000";mac_control <= '0'; c_ram_write_address <= "00"; 
		c_I <= "00"; c_J <= "00"; c_a <= "00";
		-- Above signals remain unchanged in this state
		ram_read <= '1'; rom_read <= '1'; 
		-- Read operations from RAM and ROM are enabled
		IF I = "01110111" THEN
			c_rom_address <= "000"; c_ram_read_address <= "000"; c_b <= "00";
			-- If I == 159, there is no need to change signals as we have done filtering and go to the default state
		ELSE
			c_rom_address <= "111"; c_ram_read_address <= "011"; c_b <= "11";
			-- Increment the rom_address, Increment the ram_address, Increment b
		END IF;
	ELSIF state = S2 THEN
		ram_read <= '1'; ram_write <= '0'; 
		rom_read <= '1'; mac_control <= '1'; c_ram_write_address <= "00";
		-- Read signals are enabled, MAC operations are enabled, Write is disabled
		IF a = "10" AND b = "10" THEN
			-- If filtering for a pixel is done
			c_a <= "01"; c_b <= "01"; c_rom_address <= "101";
			-- Set a = 0, b = 0, Decrement rom_address by 8 (To go back from 8 to 0)
			IF J = "10011110" THEN
				-- If J == 158
				c_I <= "11"; c_J <= "01"; c_curr_address <= "011"; c_ram_read_address <= "011";
				-- Increment I by 1 (go to the next row)
				-- Set J to 1 (Column 1 of next row)
				-- Increment curr_address by 3 (As we skip 2 edge pixels in between)
				-- Determine the new ram_read_address using the curr_address
			ELSE
				c_J <= "11"; c_I <= "00"; c_curr_address <= "101"; c_ram_read_address <= "101";
				-- I remains unchanged
				-- Increment J by 1 (go to next column)
				-- Increment curr_address by 1
				-- Determine the new ram_read_address using the curr_address
			END IF;
		ELSIF b = "10" THEN
			-- If b == 2
			-- When we reach end of row of small matrix while filtering
			c_I <= "00"; c_J <= "00";
			-- I and J remain unchanged
			c_a <= "11"; c_b <= "01"; c_rom_address <= "111";
			-- a is incremented by 1 and b is set to 0 (First column of next row)
			-- rom_address is incremented by 1
			c_ram_read_address <= "111"; c_curr_address <= "000";
			-- Determine the new ram_read_address by adding 158 to it
			-- curr_address remains unchanged
		ELSE
			-- We have not reached end of row of small matrix while filtering
			c_I <= "00"; c_J <= "00"; c_a <= "00"; c_b <= "11";
			-- I, J and a remain unchanged, b is incremented by 1 (next column)
			c_rom_address <= "111";
			-- rom_address is incremented by 1
			c_ram_read_address <= "110"; c_curr_address <= "000";
			-- ram_read_address is incremented by 1
			-- curr_address remains unchanged
		END IF;
	ELSE
		-- State S3
		ram_read <= '0'; ram_write <= '1'; c_ram_read_address <= "000"; c_curr_address <= "000";
		rom_read <= '0'; mac_control <= '0'; c_ram_write_address <= "11";
		-- Read is disabled and Write is enabled
		-- ram_write_address is incremented by 1 for next pixel
		c_I <= "00"; c_J <= "00"; c_a <= "00"; c_b <= "00"; c_rom_address <= "000";
	END IF;
END PROCESS;
-----------------End of Control Part----------------------------------------------------

-----------------Start of Data Path-----------------------------------------------------

-- Clock dependant assignments
PROCESS (clk) BEGIN
	IF clk'EVENT AND clk = '1' THEN
		IF c_I(0) = '1' THEN 
			IF c_I = "01" THEN I <= "00000001";
			-- Initial value is 1
			ELSE I <= I+1;
			END IF;
		END IF;
		IF c_J(0) = '1' THEN 
			IF c_J = "01" THEN J <= "00000001";
			-- Initial value is 1
			ELSE J <= J+1;
			END IF;
		END IF;
		IF c_a(0) = '1' THEN 
			IF c_a = "01" THEN a <= "00";
			-- Initial value is 0
			ELSE a <= a+1;
			END IF;
		END IF;
		IF c_b(0) = '1' THEN 
			IF c_b = "01" THEN b <= "00";
			-- Initial value is 0
			ELSE b <= b+1;
			END IF;
		END IF;
		IF c_curr_address(0) = '1' THEN
			IF c_curr_address = "001" THEN curr_address <= "0000000010100001";
			-- Initial value of this is 161 (160*1 + 1)
			ELSIF c_curr_address = "011" THEN curr_address <= curr_address + 3;
			ELSE curr_address <= curr_address + 1;
			END IF;
		END IF;
		IF c_rom_address(0) = '1' THEN
			IF c_rom_address = "001" THEN rom_address <= "00000";
			ELSIF c_rom_address = "011" THEN rom_address <= "01001";
			ELSIF c_rom_address = "101" THEN rom_address <= rom_address - 8;
			ELSE rom_address <= rom_address + 1;
			END IF;
		END IF;
		IF c_ram_read_address = "110" THEN ram_read_address <= ram_read_address + 1;
		ELSIF c_ram_read_address(0) = '1' THEN
			IF c_ram_read_address = "001" THEN ram_read_address <= "0000000000000000";
			ELSIF c_ram_read_address = "011" THEN ram_read_address <= curr_address - 158;
			ELSIF c_ram_read_address = "101" THEN ram_read_address <= curr_address - 160;
			ELSE ram_read_address <= ram_read_address + 158;
			END IF;
		END IF;
		IF c_ram_write_address(0) = '1' THEN
			IF c_ram_write_address = "01" THEN ram_write_address <= "1000000000000000";
			-- Initial value is 32768
			ELSE ram_write_address <= ram_write_address + 1;
			END IF;
		END IF;
	END IF;
END PROCESS;


-- Concurrent assignments
C1 : RAM_64Kx8 PORT MAP (clock => clk, read_enable => ram_read, write_enable => ram_write, 
	address => ram_address, data_in => ram_in, data_out => ram_out);
-- RAM port map
C2 : ROM_32x9 PORT MAP (clock => clk, read_enable => rom_read, address => final_rom_address, 
	data_out => rom_out);
-- ROM port map
C3 : MAC PORT MAP (clock => clk, control => mac_control, data_in1 => mac_in1, 
	data_in2 => mac_in2, data_out => mac_out);
-- MAC port map

WITH state SELECT mac_in1 <=
	"000000000000000000" WHEN S1,
	"0000000000" & ram_out WHEN OTHERS;
-- When in S1, it is set to 0 to initialize internal signal "sum" of the MAC to 0
-- Else it takes the value of ram_out

mac_in2 <= 
	"111111111" & rom_out WHEN rom_out(8) = '1'
	ELSE "000000000" & rom_out;
-- Append a series of 1s to the left if coefficient is negative
-- 0s are appended if coefficient is positive

ram_in <= 
	"00000000" WHEN mac_out(17) = '1'
	ELSE mac_out(14 downto 7);
-- 0 when mac_out is negative, else it discards the 7 leftmost bits and 3 rightmost bits

WITH state SELECT ram_address <=
	std_logic_vector(ram_write_address) WHEN S3,
	std_logic_vector(ram_read_address) WHEN OTHERS;
-- Selects ram_write_address in S3 (Write state) and ram_read_address otherwise

final_rom_address <= std_logic_vector(rom_address);
-- Typecasting to input to the ROM port

END ARCH;
-----------------End of Data Path-------------------------------------------------------

-----------------End of Main Architecture-----------------------------------------------