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
      
      //Load in json data
			d3.json("https://raw.githubusercontent.com/aagoldberg/aagoldberg.github.io/master/cb_2016_us_cd115_5m.json", function(json) {
				//Set input domain for color scale
				color.domain([
					d3.min(json, function(d) { return d.HRCper; }), 
					d3.max(json, function(d) { return d.HRCper; })
				]);			

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
			
			};