pragma Warnings (Off);
with Interfaces;
with HAL; use HAL;
pragma Warnings (On);

package STM32 is
   pragma Pure;

   type GPIO_Alternate_Function is private;

private
   type GPIO_Alternate_Function is new UInt4;

end STM32;
