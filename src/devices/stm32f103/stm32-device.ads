with STM32_SVD; use STM32_SVD;

with STM32.GPIO; use STM32.GPIO;

package STM32.Device is
   pragma Elaborate_Body;

   type Debug_Ports is
     (Full_JTAG_SWDP, Full_JTAG_SWDP_wo_NJTRST, Only_SWDP, Disabled);

   for Debug_Ports use
     (Full_JTAG_SWDP           => 2#000#,
      Full_JTAG_SWDP_wo_NJTRST => 2#001#,
      Only_SWDP                => 2#010#,
      Disabled                 => 2#100#);

   for Debug_Ports'Size use 3;

   procedure Configure_Debug_Ports (Mode : Debug_Ports);

   procedure Enable_Clock (This : aliased in out GPIO_Port);
   procedure Enable_Clock (Point : GPIO_Point);
   procedure Enable_Clock (Points : GPIO_Points);

   GPIO_A : aliased GPIO_Port
   with Import, Volatile, Address => STM32_SVD.GPIOA_Base;
   GPIO_B : aliased GPIO_Port
   with Import, Volatile, Address => STM32_SVD.GPIOB_Base;
   GPIO_C : aliased GPIO_Port
   with Import, Volatile, Address => STM32_SVD.GPIOC_Base;

   PC13 : aliased GPIO_Point := (GPIO_C'Access, Pin_13);
private
end STM32.Device;
