with System; use System;
with STM32_SVD.RCC; use STM32_SVD.RCC;
with STM32_SVD.AFIO; use STM32_SVD.AFIO;

package body STM32.Device is

   --  HSI_VALUE : constant := 48_000_000;

   procedure Configure_Debug_Ports (Mode : Debug_Ports) is
      pragma Unreferenced (Mode);
   begin
      AFIO_Periph.MAPR.SWJ_CFG := 2#010#;
   end Configure_Debug_Ports;

   procedure Enable_Clock (This : aliased in out GPIO_Port) is
   begin
      if This'Address = GPIOC_Base then
         RCC_Periph.APB2ENR.IOPCEN := True;
      end if;
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (Point : GPIO_Point) is
   begin
      Enable_Clock (Point.Periph.all);
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (Points : GPIO_Points) is
   begin
      for Point of Points loop
         Enable_Clock (Point);
      end loop;
   end Enable_Clock;

end STM32.Device;