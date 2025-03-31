with STM32.GPIO;
--  with System;         use System;
with STM32_SVD.GPIO;
use STM32_SVD.GPIO;

--  with System.Machine_Code;

package body STM32.GPIO is

   subtype GPIO_Pin_Index is Natural range 0 .. GPIO_Pin'Pos (GPIO_Pin'Last);

   --  function Any_Set (Pins : GPIO_Points) return Boolean is
   --  begin
   --     for Pin of Pins loop
   --        if Pin.Set then
   --           return True;
   --        end if;
   --     end loop;

   --     return False;
   --  end Any_Set;

   overriding
   function Mode (This : GPIO_Point) return HAL.GPIO.GPIO_Mode is
   begin
      --  TODO
      return HAL.GPIO.Output;
   end Mode;

   --  function Get_IO_Mode_From_Fields
   --    (CNFx : HAL.UInt2; MODEx : HAL.UInt2) return Pin_IO_Modes
   --  is
   --     IO_Mode : Pin_IO_Modes;
   --  begin
   --     if MODEx = 2#00# then
   --        if CNFx = 2#00# then
   --           IO_Mode := Mode_Analog;
   --        else
   --           IO_Mode := Mode_In;
   --        end if;
   --     else
   --        if CNFx = 2#00# or CNFx = 2#01# then
   --           IO_Mode := Mode_Out;
   --        else
   --           IO_Mode := Mode_AF;
   --        end if;
   --     end if;

   --     return IO_Mode;
   --  end Get_IO_Mode_From_Fields;

   --  RM0008.pdf p. 156, Table 20. Port bit configuration table
   --  Mode_In:
   --    MODEx = 2#00#
   --  Mode_Analog:
   --    Mode_In and CNFx = 2#00#;
   --  Mode_AF:
   --    not Mode_In and (CNFx = 2#10# or CNFx = 2#11#)
   --  Mode_Out:
   --    not Mode_In and (CNFx = 2#00# or CNFx = 2#01#)

   function Pin_IO_Mode (This : GPIO_Point) return Pin_IO_Modes is
   begin
      --  case This.Pin is

      --     -- GPIOx_CRL (RM0008.pdf p. 171)
      --     when Pin_0 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRL.CNF0,
      --                                        This.Periph.CRL.MODE0);
      --     when Pin_1 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRL.CNF1,
      --                                        This.Periph.CRL.MODE1);
      --     when Pin_2 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRL.CNF2,
      --                                        This.Periph.CRL.MODE2);
      --     when Pin_3 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRL.CNF3,
      --                                        This.Periph.CRL.MODE3);
      --     when Pin_4 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRL.CNF4,
      --                                        This.Periph.CRL.MODE4);
      --     when Pin_5 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRL.CNF5,
      --                                        This.Periph.CRL.MODE5);
      --     when Pin_6 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRL.CNF6,
      --                                        This.Periph.CRL.MODE6);
      --     when Pin_7 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRL.CNF7,
      --                                        This.Periph.CRL.MODE7);

      --        -- GPIOx_CRH (RM0008.pdf p. 172)
      --     when Pin_8 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRH.CNF8,
      --                                        This.Periph.CRH.MODE8);
      --     when Pin_9 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRH.CNF9,
      --                                        This.Periph.CRH.MODE9);
      --     when Pin_10 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRH.CNF10,
      --                                        This.Periph.CRH.MODE10);
      --     when Pin_11 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRH.CNF11,
      --                                        This.Periph.CRH.MODE11);
      --     when Pin_12 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRH.CNF12,
      --                                        This.Periph.CRH.MODE12);
      --     when Pin_13 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRH.CNF13,
      --                                        This.Periph.CRH.MODE13);
      --     when Pin_14 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRH.CNF14,
      --                                        This.Periph.CRH.MODE14);
      --     when Pin_15 =>
      --        return Get_IO_Mode_From_Fields (This.Periph.CRH.CNF15,
      --                                        This.Periph.CRH.MODE15);
      --  end case;
      return Mode_Out;
   end Pin_IO_Mode;

   -----------------
   -- Set_IO_Mode --
   -----------------

   procedure Set_IO_Mode (This : GPIO_Point; CNFx, MODEx : HAL.UInt2) is
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
   begin
      if Index < 8 then
         This.Periph.CRL.CNF_MODE.Arr (Index).CNFx := CNFx;
         This.Periph.CRL.CNF_MODE.Arr (Index).MODEx := MODEx;
      else
         This.Periph.CRH.CNF_MODE.Arr (Index - 8).CNFx := CNFx;
         This.Periph.CRH.CNF_MODE.Arr (Index - 8).MODEx := MODEx;
      end if;
   end Set_IO_Mode;

   overriding
   procedure Set_Mode
     (This : in out GPIO_Point; Mode : HAL.GPIO.GPIO_Config_Mode) 
   is
   begin
      null;
   end Set_Mode;

   overriding
   function Pull_Resistor
     (This : GPIO_Point) return HAL.GPIO.GPIO_Pull_Resistor is
   begin
      return HAL.GPIO.Floating;
   end Pull_Resistor;

   overriding
   procedure Set_Pull_Resistor
     (This : in out GPIO_Point; Pull : HAL.GPIO.GPIO_Pull_Resistor) is
   begin
      null;
   end Set_Pull_Resistor;

   ---------
   -- Set --
   ---------

   overriding
   function Set (This : GPIO_Point) return Boolean is
      Pin_Mask : constant UInt16 := GPIO_Pin'Enum_Rep (This.Pin);
   begin
      return (This.Periph.IDR.IDR.Val and Pin_Mask) = Pin_Mask;
   end Set;

   ---------
   -- Set --
   ---------

   overriding
   procedure Set (This : in out GPIO_Point) is
   begin
      This.Periph.BSRR.BS.Val := GPIO_Pin'Enum_Rep (This.Pin);
   end Set;

   -----------
   -- Clear --
   -----------

   overriding
   procedure Clear (This : in out GPIO_Point) is
   begin
      This.Periph.BSRR.BR.Val := GPIO_Pin'Enum_Rep (This.Pin);
   end Clear;

   ------------
   -- Toggle --
   ------------

   overriding
   procedure Toggle (This : in out GPIO_Point) is
   begin
      This.Periph.ODR.ODR.Val :=
        This.Periph.ODR.ODR.Val xor GPIO_Pin'Enum_Rep (This.Pin);
   end Toggle;

   -------------
   -- Any_Set --
   -------------

   function Any_Set (Pins : GPIO_Points) return Boolean is
   begin
      for Pin of Pins loop
         if Pin.Set then
            return True;
         end if;
      end loop;

      return False;
   end Any_Set;

   -------------
   -- All_Set --
   -------------

   function All_Set (Pins : GPIO_Points) return Boolean is
   begin
      for Pin of Pins loop
         if not Pin.Set then
            return False;
         end if;
      end loop;

      return True;
   end All_Set;

   ---------
   -- Set --
   ---------

   procedure Set (Pins : in out GPIO_Points) is
   begin
      for Pin of Pins loop
         Pin.Set;
      end loop;
   end Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (Pins : in out GPIO_Points) is
   begin
      for Pin of Pins loop
         Pin.Clear;
      end loop;
   end Clear;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Points : in out GPIO_Points) is
   begin
      for Point of Points loop
         Point.Toggle;
      end loop;
   end Toggle;

   -----------
   -- Drive --
   -----------

   procedure Drive (This : in out GPIO_Point; Condition : Boolean) is
   begin
      if Condition then
         This.Set;
      else
         This.Clear;
      end if;
   end Drive;

   ------------
   -- Locked --
   ------------

   function Locked (This : GPIO_Point) return Boolean is
      Mask : constant UInt16 := GPIO_Pin'Enum_Rep (This.Pin);
   begin
      return (This.Periph.LCKR.LCK.Val and Mask) = Mask;
   end Locked;

   -----------------------
   -- Configure_Pin_Out --
   -----------------------

   procedure Configure_Pin_Out
     (Point    : GPIO_Point;
      Out_Type : Pin_Output_Types;
      Speed    : Pin_Output_Speeds)
   is
      CNFx_Field, MODEx_Field : HAL.UInt2;
   begin
      MODEx_Field := Pin_Output_Speeds'Enum_Rep (Speed);
      CNFx_Field := Pin_Output_Types'Enum_Rep (Out_Type);

      Set_IO_Mode (Point, CNFx_Field, MODEx_Field);
   end Configure_Pin_Out;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO (This : GPIO_Point; Config : GPIO_Port_Configuration)
   is
   begin
      case Config.Mode is
         when Mode_In | Mode_Analog =>
            null;

         when Mode_Out =>
            This.Configure_Pin_Out (Config.Output_Type, Config.Speed);

         when Mode_AF =>
            null;
      end case;
   end Configure_IO;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (Points : GPIO_Points; Config : GPIO_Port_Configuration) is
   begin
      for Point of Points loop
         Point.Configure_IO (Config);
      end loop;
   end Configure_IO;

end STM32.GPIO;
