pragma Restrictions (No_Elaboration_Code);

package STM32.Device_Id is
   
   subtype Device_Id_Image is String (1 .. 12);

   function Unique_Id return Device_Id_Image;

   type Device_Id_Tuple is array (1 .. 3) of UInt32
      with Component_Size => 32;

   function Unique_Id return Device_Id_Tuple;

end STM32.Device_Id;