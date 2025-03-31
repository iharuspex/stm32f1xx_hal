with System;

package body STM32.Device_Id is
   
   ID_Address : constant System.Address := System'To_Address (16#1FFF_F7EB#);

   function Unique_Id return Device_Id_Image is
      Result : Device_Id_Image;
      for Result'Address use ID_Address;
      pragma Import (Ada, Result);
   begin
      return Result;
   end Unique_Id;

   function Unique_Id return Device_Id_Tuple is
      Result : Device_Id_Tuple;
      for Result'Address use ID_Address;
   begin
      return Result;
   end Unique_Id;

end STM32.Device_Id;