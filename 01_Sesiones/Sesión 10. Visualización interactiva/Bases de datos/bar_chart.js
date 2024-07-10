// !preview r2d3 data=c(
//   list(Producto="A", Ventas=30),
//   list(Producto="B", Ventas=80),
//   list(Producto="C", Ventas=45),
//   list(Producto="D", Ventas=60),
//   list(Producto="E", Ventas=20),
//   list(Producto="F", Ventas=90),
//   list(Producto="G", Ventas=55)
// )

var margin = {top: 20, right: 30, bottom: 40, left: 90},
    width = width - margin.left - margin.right,
    height = height - margin.top - margin.bottom;

var svg = d3.select("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

var x = d3.scaleLinear()
  .range([0, width]);

var y = d3.scaleBand()
  .range([height, 0])
  .padding(0.1);

x.domain([0, d3.max(data, function(d) { return d.Ventas; })]);
y.domain(data.map(function(d) { return d.Producto; }));

svg.selectAll(".bar")
  .data(data)
  .enter().append("rect")
  .attr("class", "bar")
  .attr("width", function(d) { return x(d.Ventas); })
  .attr("y", function(d) { return y(d.Producto); })
  .attr("height", y.bandwidth());

svg.append("g")
  .attr("class", "x axis")
  .attr("transform", "translate(0," + height + ")")
  .call(d3.axisBottom(x));

svg.append("g")
  .attr("class", "y axis")
  .call(d3.axisLeft(y));
