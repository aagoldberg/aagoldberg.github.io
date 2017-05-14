//Width and height
			var w = 500;
			var h = 300;
			//Define map projection
			var projection = d3.geo.albersUsa()
								   .translate([w/2, h/2])
								   .scale([500]);
			//Define path generator
			var path = d3.geo.path()
							 .projection(projection);
							 
			//Define quantize scale to sort data values into buckets of color
			var color = d3.scale.quantize()
								.range(["rgb(237,248,233)","rgb(186,228,179)","rgb(116,196,118)","rgb(49,163,84)","rgb(0,109,44)"]);
								//Colors taken from colorbrewer.js, included in the D3 download
			
      //Create SVG element
			var svg = d3.select("body")
						.append("svg")
						.attr("width", w)
						.attr("height", h);
			
      //Load in survey data
			d3.csv("https://raw.githubusercontent.com/aagoldberg/aagoldberg.github.io/master/PresResDat3.csv", function(data) {
				//Set input domain for color scale
				color.domain([
					d3.min(data, function(d) { return d.HRCper; }), 
					d3.max(data, function(d) { return d.HRCper; })
				]);
				
        //Load in GeoJSON data
				d3.json("https://raw.githubusercontent.com/aagoldberg/aagoldberg.github.io/master/cb_2016_us_cd115_5m.json", function(json) {
					//Merge the survey data and GeoJSON
					//Loop through once for each survey data value
					for (var i = 0; i < data.length; i++) {
				
						//Grab state name and congressional district
						var dataState = data[i].State;
						var dataCD = data[i].CD;

						//Grab data value, and convert from string to float
						var dataValue = parseFloat(data[i].HRCper);
				
						//Find the corresponding state inside the GeoJSON
						for (var j = 0; j < json.features.length; j++) {
						
							var jsonState = json.features[j].properties.STATEFP;
							var jsonCD = json.features[j].properties.CD115FP;
							
							if (dataState == jsonState && dataCD == jsonCD) {
						
								//Copy the data value into the JSON
								json.features[j].properties.HRCper = dataValue;
								
								//Stop looking through the JSON
								break;
								
							}
						}		
					}
					//Bind data and create one path per GeoJSON feature
					svg.selectAll("path")
					   .data(json.features)
					   .enter()
					   .append("path")
					   .attr("d", path)
					   .style("fill", function(d) {
					   		//Get data value
					   		var value = d.properties.HRCper;
					   		
					   		if (value) {
					   			//If value exists…
						   		return color(value);
					   		} else {
					   			//If value is undefined…
						   		return "rgb(211,216,211)";
					   		}
					   });
			
				});
			
			});