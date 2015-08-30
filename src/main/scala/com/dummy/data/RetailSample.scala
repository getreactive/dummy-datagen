package com.dummy.data

import java.io.{File, FileOutputStream, PrintWriter}
import java.util.{TimeZone, Calendar, Date}

import scala.util.Random

/**
 * Created by rahul on 29/08/15.
 */
object RetailSample {


  def getTimestamp(calendar:Calendar,month:Int,day:Int,hour:Int):Long={
    calendar.clear();
    calendar.set(2015, month, day);
    return ((calendar.getTimeInMillis() / 1000L) + (3600*hour) + 600);
  }

  def main(args: Array[String]) {



    val headers = "timestamp,state,store,category,item,quantity,sales,year,month,day,hour"

    val states ="""Alabama,
                  Alaska,
                  Arizona,
                  Arkansas,
                  California,
                  Colorado,
                  Connecticut,
                  Delaware,
                  Florida,
                  Georgia,
                  Hawaii,
                  Idaho,
                  Illinois,
                  Indiana,
                  Iowa,
                  Kansas,
                  Kentucky,
                  Louisiana,
                  Maine,
                  Maryland,
                  Massachusetts,
                  Michigan,
                  Minnesota,
                  Mississippi,
                  Missouri,
                  Montana,
                  Nebraska,
                  Nevada,
                  New Hampshire,
                  New Jersey,
                  New Mexico,
                  New York,
                  North Carolina,
                  North Dakota,
                  Ohio,
                  Oklahoma,
                  Oregon,
                  Pennsylvania,
                  Rhode Island,
                  South Carolina,
                  South Dakota,
                  Tennessee,
                  Texas,
                  Utah,
                  Vermont,
                  Virginia,
                  Washington,
                  West Virginia,
                  Wisconsin,
                  Wyoming""".split(",")

    val category_map = Map("Beverages" -> Array("Chai","Chang","Chartreuse verte","Cote de Blaye","Guarana Fantastica","Ipoh Coffee","Lakkalikoori","Laughing Lumberjack","Outback Lager","Rhonbrau Klosterbier","Sasquatch Ale","Steeleye Stout"),
      "Condiments" -> Array("Aniseed Syrup","Chef Anton's Cajun Seasoning","Chef Anton's Gumbo Mix","Genen Shouyu","Grandma's Boys Spread","Gula Malacca","Vegie-Spread","Sirop d'erable","Northwoords Cranberry Sauce"),
      "Confections" -> Array("Chocolade","Zaanse koeken","Valkoinen suklaa","Tarte au sucre","Pavlova","Teatime Chocoloate Biscuits","Schoggi Schokolade","Gumbar Gummibarchen"),
      "Dairy Products" -> Array("Camembert Pierrot","Flotemysost","Geitost","Gorgonzola Telino","Gudbrandsdalsost","Queso Cabrales","Mascarpone Fabioli","Mozzarella di Giovanni") ,
      "Grains/Cereals" -> Array("Filo Mix","Gnocchi di nonna Alice","Gustaf's Knackbrod","Ravioli Angelo","Tunnbrod","Singaporean Hokkien Fried Mee","Wimmers gute Semmelknodel"),
      "Meat/Poultry" -> Array("Alice Mutton","Mishi Kobe Niku","Perth Pastries","Pate chinois","Tourtiere","Thuringer Rostbratwurst"),
      "Produce" -> Array("Longlife Tofu","Manjimup Dried Apples","Rossle Sauerkraut","Tofu","Uncle Bob's Organic Dried Pears"),
      "Seafood" -> Array("Boston Crab Meat","Carnarvon Tigers","Escargots de Bourgogne","Gravad Iax","Ikura","Inlagd Sill","Jack's new England Clam Chowder","Konbu","Rogede sild","Rod Kaviar","Spegesild"))


    val cat_keys = category_map.keys.toArray

    val currentdatetime = new Date()
    val filename = "datafile" + currentdatetime.getTime +".csv"
    val writer = new PrintWriter(new FileOutputStream(new File(filename),true))
    val calendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
    writer.append(headers+"\n")

    //For each State generate random unique store

    val NUMOFLINES: Int = 20
    val NUMDAYS: Int = 7
    val NUMHOURS: Int = 24

    val year: Int = 2015
    val month: Int = 6
    val first_day: Int = 0

    // State Loop :-- Record per state

    for(statename <- states ) {

      var day = first_day
      val state_name = statename.trim
      var currentday = 0

      for(currentday <- 1 until NUMDAYS) {
        day += 1
        var currenthour = 0

        for(currenthour <- 1 until NUMHOURS){
          var current_timestamp = getTimestamp(calendar, month - 1, currentday, currenthour)

          //state
          val rndForStore = new scala.util.Random
          var storerange = 0 to 2
          if(state_name == "California" || state_name == "Florida" || state_name == "New York" || state_name == "Texas" || state_name == "Illinois" || state_name == "Pennsylvania" ||state_name == "Ohio"){
            storerange = 1 to 8
          }else if(state_name=="New Jersey" || state_name == "North Carolina" || state_name == "Georgia" || state_name == "Virginia" || state_name == "Massachusetts" || state_name == "Michigan" || state_name == "Washington"){

            storerange = 1 to 3
          }
          val rndForStoreNo = storerange(rndForStore.nextInt(storerange length))

          var storeEntry = 0

          for(storeEntry <- 1 until rndForStoreNo){
             //store
             val store = "store"+Random.nextInt(200)+Random.nextInt(200)
            println(storeEntry)
            val rnd = scala.util.Random
            val rndnumber = rnd.nextInt(20)
            var itemEntry = 0

            for(itemEntry <- 1 until rndnumber){
              val rndqty = rnd.nextInt(20)
              val rndsales = rnd.nextInt(1000)
              val random_category = cat_keys(Random.nextInt(cat_keys.length))  //category
              val item_arr = category_map(random_category)
              val random_item = item_arr(Random.nextInt(item_arr.length)) //item
              var random_quantity = ""
              var random_sales = ""

              if(state_name == "California" || state_name == "Florida" || state_name == "New York" || state_name == "Texas" || state_name == "Illinois" || state_name == "Pennsylvania" ||state_name == "Ohio"){

                val highrnd = new scala.util.Random
                val qtyrange = 10 to 20
                val salerange = 3000 to 5000
                val highqty = qtyrange(highrnd.nextInt(qtyrange length))
                val highsales = salerange(highrnd.nextInt(salerange length))
                random_quantity = highqty.toString
                random_sales = highsales.toString

              }else if(state_name=="New Jersey" || state_name == "North Carolina" || state_name == "Georgia" || state_name == "Virginia" || state_name == "Massachusetts" || state_name == "Michigan" || state_name == "Washington"){
                val midrnd = new scala.util.Random
                val qtyrange = 5 to 9
                val salerange = 1000 to 2000
                val midqty = qtyrange(midrnd.nextInt(qtyrange length))
                val midsales = salerange(midrnd.nextInt(salerange length))
                random_quantity = midqty.toString
                random_sales = midsales.toString

              } else{

                val lowrnd = new scala.util.Random
                val qtyrange = 0 to 4
                val salerange = 0 to 900
                val lowqty = qtyrange(lowrnd.nextInt(qtyrange length))
                val lowsales = salerange(lowrnd.nextInt(salerange length))
                if(lowqty==0){
                  random_sales= "0"
                }else{
                  random_sales = lowsales.toString
                }
                if(lowsales == 0) {
                  random_quantity = "0"
                }else{
                  random_quantity = lowqty.toString
                }

              }

              writer.append(current_timestamp+","+state_name+","+store+","+random_category+","+random_item+","+random_quantity+","+random_sales+","+year+","+month+","+currentday+","+currenthour+"\n")
            }
            // writer.append(statename.trim+"\n")
            //println(statename.trim)

          }

        }
      }





     }

    writer.close()
  }

}
