/*
   PHYSICAL GMAIL NOTIFIER 2
   Based on a Jamie Matthews idea
   (his work: http://www.j4mie.org/2008/02/15/how-to-make-a-physical-gmail-notifier/)

       L I C E N S E :

   Attribution-Noncommercial-Share Alike 2.5 Italy
   You are free:
       * to Share — to copy, distribute and transmit the work
       * to Remix — to adapt the work
   Under the following conditions:
       * Attribution. You must attribute the work in the manner specified
         by the author or licensor (but not in any way that suggests that
         they endorse you or your use of the work).
       * Noncommercial. You may not use this work for commercial purposes.
       * Share Alike. If you alter, transform, or build upon this work, you
         may distribute the resulting work only under the same or similar
         license to this one.
       * For any reuse or distribution, you must make clear to others the
         license terms of this work. The best way to do this is with a link
         to this web page:
         http://creativecommons.org/licenses/by-nc-sa/2.5/it/deed.en
       * Any of the above conditions can be waived if you get permission from
         the copyright holder.
       * Nothing in this license impairs or restricts the author's moral rights.

       Your fair dealing and other rights are in no way affected by the above.
       This is a human-readable summary of the Legal Code.
       Full license: http://creativecommons.org/licenses/by-nc-sa/2.5/it/legalcode

       KTHXBAI
*/

int outPin = 11; // Output connected to PWM pin 11
int mail = 0; // mail count
int val; // Value read from the serial port
int pwm = 0; // Led PWM control
int myloop = 0;

void setup()
{
    Serial.begin(9600);
    Serial.flush();
}
    
   void loop()
   {
   // Read from serial port
   if (Serial.available())
   {
     val = Serial.read();
     Serial.println(val);
     if ((val > 48) && (val < 59)) mail = val-48;
       // 48=0mails 49=1mail 50=2 ... 57=9 58=: (: is more than 9 mails, so we keep led always on)     
     else mail = 0;
   }
   if (mail == 0) analogWrite(outPin,0);
   if ((mail > 0) && (mail < 10)) 
    {  
      for(myloop = 1; myloop <= mail; myloop+=1) // loops N times (N = new mail count)
      {
        for(pwm = 0 ; pwm <= 255; pwm+=5) // fade in (from min to max) 
        { 
          analogWrite(outPin, pwm);           // sets the value (range from 0 to 255) 
          delay(20);                            // waits for 30 milli seconds to see the dimming effect 
        } 
        for(pwm = 255; pwm >=0; pwm-=5)   // fade out (from max to min) 
        { 
          analogWrite(outPin, pwm); 
          delay(20); 
        }
          
      }
      delay(2000); // waits 2 seconds before beginning next loop
     }
    if (mail == 10) analogWrite(outPin,40); //or if there is A LOT of mail, it keeps led on      
   }
