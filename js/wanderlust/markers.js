// General notes:
//   - Resize images to be 300px on the largest dimension
const markers = [
  // United States
  // -- Arizona
  {
    name: "Biosphere 2",
    description: "Its initial goal was to simulate a close environment where a \
     group of 6 humans should survive for 2 years without any exchange with \
     the exterior. They had different biomes, including a rain forest, an \
     ocean to a desert.</br></br > \
     The plan didn’t go well because they had missed interaction of the plants \
     in one of the seasons, which causes the level of oxygen to fall under the \
     accepted level, so they had to get extern supply. The mission did last 2 \
     years, but was not a perfect closed environment.</br ></br > \
     Today it's a museum open to public.",
    lat: 32.57911585980931,
    lng:  -110.85138782115112,
    img_name: "biosphere.jpeg",
    photographer: "Johndedios",
    img_link: "https://commons.wikimedia.org/wiki/File:Wiki_bio2_sunset_001.jpg",
    visited: true,
  },
  {
    name: "Cactus Forest",
    description: "Cacti from the Saguaro National Park",
    lat: 32.23041339658249,
    lng: -111.14510525157992,
    img_name: "saguaro.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/CZYx6drYd2pn7dp59",
    visited: true,
    category: "nature",
  },
  {
    name: "Chiricahua National Monument",
    description: "Amazing rock formations in Southeastern Arizona",
    lat: 32.0135714,
    lng: -109.3422953,
    img_name: "chiricahua.png",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/ZdNaFY3BsmLqowky6",
    visited: true,
    category: "nature",
  },
  {
    name: "Antelope Canyon",
    description:
      "Antelope Canyon is a slot canyon in the American Southwest, on Navajo land east of Page, Arizona.",
    lat: 36.86470241618197,
    lng: -111.37357693817302,
    img_name: "antelope_canyon.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/dapDGNR45KABBqk77",
    visited: true,
    category: "nature",
  },
  {
    name: "Grand Canyon",
    description:
      "One of the most famous canyons in the world, carved by the Colorado river.",
    lat: 36.06714174570464,
    lng: -112.11638907022639,
    img_name: "grand_canyon.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/ZdNaFY3BsmLqowky6",
    visited: true,
    category: "nature",
  },
  {
    name: "Havasu Falls",
    description:
      "Havasu Falls is a waterfall of Havasu Creek, located in the Grand Canyon, Arizona, United States. It is within Havasupai tribal lands.",
    lat: 36.25549664057389,
    lng: -112.6978814550031,
    category: "nature",
  },
  {
    name: "Horseshoe Bend",
    description:
      "Horseshoe Bend is a horseshoe-shaped incised meander of the Colorado River located near the town of Page, Arizona, United States.",
    lat: 36.88032695559509,
    lng: -111.51014454598656,
    img_name: "horseshoe_bend.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/4HVTnDDqfNaLEcxN6",
    visited: true,
    category: "nature",
  },
  {
    name: "Monument Valley",
    description:
      "Famous natural area on the Navajo Nation Reservation features towering red sandstone buttes.",
    lat: 36.99818269582502,
    lng: -110.09847885332755,
    photographer: "me",
    img_name: "monument_valley.jpg",
    visited: true,
    img_link: "https://photos.app.goo.gl/MGip7ujWszfviikH8",
    category: "nature",
  },
  // -- California
  {
    name: "Ancient Bristlecone Pine Forest",
    description: "Oldest Trees in the world",
    lat: 37.38079104393285,
    lng: -118.16166570419409,
    img_name: "bristlecone.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/ZdNaFY3BsmLqowky6",
    visited: true,
    category: "nature",
  },
  {
    name: "Cholla Garden",
    description: "Garden of the cactus plant known as Teddy Bear Cholla in Joshua Tree National Park",
    lat: 33.925661614092725,
    lng: -115.92893906366216,
    img_name: "cholla.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/ZdNaFY3BsmLqowky6",
    visited: true,
    category: "nature",
  },
  {
    name: "Cinder Cone at Lassen National Park",
    description:
      "The cone was built to a height of 230 m above the surrounding area. Then, like many cinder cones, it was snuffed out when several basalt lava flows erupted from its base",
    lat: 40.547385,
    lng: -121.319973,
    img_name: "cinder_cone.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/wHVMR95chNLKy9Pw6",
    visited: true,
    category: "nature",
  },
  {
    name: "Forty-Nine Palm Oasis",
    description: "Palm Oasis in the middle of the desert in Joshua Tree National Park",
    lat: 34.10556608298197,
    lng: -116.1047898389797,
    img_name: "palm_oasis.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/R5cqizWjVU8Dr6Rv6",
    visited: true,
    category: "nature",
  },
  {
    name: "Red Rock Canyon State Park",
    description: "Joshua Trees and Red Rock formations",
    lat: 35.3975446,
    lng: -117.9541879,
    img_name: "red_rock_canyon.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/99T2WX6ybRqHjJ719",
    visited: true,
    category: "nature",
  },
  {
    name: "Redwoods National Park",
    description: "Old growth Redwood forest. Home of the tallest trees in the world",
    lat: 41.40104620211404,
    lng: -124.06586779793305,
    img_name: "redwoods.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/5RGB4E4B4wq5VmHb8",
    visited: true,
    category: "nature",
  },
  {
    name: "Sequoia National Park",
    description: "Home of the giant sequoias, the most massive tree on earth",
    lat: 36.58188237927149,
    lng: -118.75141758773634,
    img_name: "sequoia.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/djZfMp6qSffVyJDe8",
    visited: true,
    category: "nature",
  },
  // -- Colorado
  {
    name: "Cliff Palace in Mesa Verde National Park",
    description:
      "Cliff dwelling built by Ancestral Puebloans from 1190 to 1260.",
    lat: 37.16779555670772,
    lng: -108.47332428961647,
    img_name: "cliff_palace.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/XfF5UVdxJZwFsjTK7",
    visited: true,
    category: "nature",
  },
  {
    name: "Great Sand Dunes National Park",
    description:
      "The Park conserves an area of large sand dunes up to 230 m tall.",
    lat: 37.75264382446697,
    lng:  -105.53244945971352,
    img_name: "great_sand_dunes.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/XfF5UVdxJZwFsjTK7",
    visited: true,
    category: "nature",
  },
  // -- DC
  {
    name: "Museums of Washington DC",
    description:
      "Washington DC has many wonderful (and free) museums, including the Smithsonian National Museum of Natural History, National Air and Space Museum, National Gallery of Art",
    lat: 38.89133974746184,
    lng: -77.02611265939058,
    img_name: "smithsonian.jpg",
    photographer: "me",
    visited: true,
    category: "museum",
  },
  // -- Florida
  {
    name: "Everglades National Park",
    description:
      "Everglades is made up of coastal mangroves, sawgrass marshes and pine flatwoods that are home to hundreds of animal species.",
    lat: 25.396406439434976,
    lng: -80.58298255104243,
    category: "nature",
  },
  // -- Hawaii
  {
    name: "Kalalau Valley",
    description:
      "The Kalalau Valley is located on the northwest side of the island of Kauaʻi in the state of Hawaii.",
    lat: 22.1799987,
    lng: -159.6711207,
    img_name: "kalalau.jpg",
    photographer: "me",
    visited: true,
    img_link: "https://photos.app.goo.gl/DMgwtGwckVr9BMY99",
    category: "nature",
  },
  // -- Illinois
  {
    name: "Field Museum",
    description:
      "Museum of Natural History in Chicago. Most artifacts from Hopewell Culture located here.",
    lat: 41.867044001846594,
    lng: -87.61689466825753,
  },
  // -- Massachusetts
  {
    name: "Museum of Fine Arts, Boston",
    description: "Neoclassical & modern wings house a vast collection from ancient Egyptian to contemporary American",
    lat: 42.33966531054568,
    lng: -71.09397264998303,
    img_name: "washington.jpg",
    visited: true,
    photographer: "me",
    img_link: "https://photos.app.goo.gl/NpxUNc9HDqBSTF5g8",
  },

  // -- New Mexico
  {
    name: "Chaco Culture National Historical Park",
    description:
      "Ruins from Puebloan people.",
    lat: 36.05536703154945,
    lng: -107.95712276204746,
    category: "nature",
  },
  {
    name: "Rock Formations of San Juan County",
    description:
      "Famous ones include King of Wings and The Alien Chair.",
    lat: 36.1730792611538,
    lng: -107.97276445191888,
    category: "nature",
  },
  {
    name: "Taos Pueblo",
    description:
      "Ancient pueblo belonging to a Taos-speaking (Tiwa) Native American tribe of Puebloan people.",
    lat: 36.43944823586406,
    lng: -105.5456227355632,
    img_name: "taos.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/GPRJLFJBfcRdmyUA9",
    visited: true,
    category: "nature",
  },
  {
    name: "White Sands National Park",
    description:
      "Landscape filled with gypsum sand dunes.",
    lat: 32.78778371883236,
    lng: -106.32397942084717,
    img_name: "white_sands.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/3XzToUVCc86GRhAc6",
    visited: true,
    category: "nature",
  },
  // -- New York
    {
    name: "Adirondack Mountains",
    description:
      "Beautiful scenery to be seen in the Fall.",
    lat: 44.14176770197113,
    lng: -73.86779255887143,
    category: "nature",
  },
  {
    name: "American Museum of Natural History",
    description:
      "From dinosaurs to outer space and everything in between, this huge museum showcases natural wonders.",
    lat: 40.78187652923415,
    lng: -73.97385945289521,
    img_name: "amnh.jpg",
    photographer: "me",
    visited: true,
    category: "museum",
  },
  {
    name: "Niagara Falls",
    description:
      "The most famous waterfalls in the US.",
    lat: 43.08406371690949,
    lng: -79.06502726278184,
    category: "nature",
  },


  // Texas
  {
    name: "Santa Elena Canyon",
    description: "Giant walls surrounding the Rio Grande, in the border of the US and Mexico",
    lat: 29.172282302934672,
    lng: -103.60246473625729,
    img_name: "santa_elena_canyon.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/E6sHVmWXjsjrUJxA8",
    visited: true,
    category: "nature",
  },
  // -- Utah
  {
    name: "Arches National Park",
    description:
      "Known as the site of more than 2,000 natural sandstone arches",
    lat: 38.74641197424302,
    lng: -109.49905024279148,
    img_name: "arches.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/2DrT2oaVXpBzLZAb9",
    visited: true,
    category: "nature",
  },
  {
    name: "Bryce Canyon National Park",
    description:
      "Crimson-colored hoodoos, which are spire-shaped rock formations",
    lat: 37.566307868258306,
    lng: -112.21969399706641,
    img_name: "bryce.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/FkoLUPan3tVhT2go9",
    visited: true,
    category: "nature",
  },
    {
    name: "Canyonlands National Park",
    description:
      "Known for its dramatic desert landscape carved by the Colorado River",
    lat: 38.32102101074824,
    lng: -109.83546234153653,
    img_name: "canyonlands.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/iwmeuuXo4ERiJR1TA",
    visited: true,
    category: "nature",
  },
  {
    name: "The Wave",
    description: "Unique Rock Formation in Southern Utah",
    lat: 36.99593,
    lng: -112.00625,
    img_name: "the_wave.jpg",
    img_link: "https://photos.app.goo.gl/aS75Weo77a6bFmTn6",
    visited: true,
    category: "nature",
  },
  // -- Washington
  {
    name: "Diablo Lake",
    description: "Cyan blue lake in North Cascades National Park.",
    lat: 48.7142378,
    lng: -121.1355309,
    img_name: "diablo_lake.png",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/HDAc12RUxrA5ndTm6",
    visited: true,
    category: "nature",
  },
  // -- Wyoming
  {
    name: "Yellowstone National Park",
    description: "Colorful hot springs and geysers",
    lat: 44.525121,
    lng: -110.8403384,
    img_name: "yellowstone.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/jWoAySZx9G2UtHGf6",
    visited: true,
    category: "nature",
  },

  // Algeria
  {
    name: "Timgad",
    description: "Timgad was a Roman city in the Aurès Mountains of Algeria. It was founded by the Roman Emperor Trajan around CE 100.",
    lat: 35.49726639943841,
    lng: 6.469364172911276,
    category: "monument",
  },

  // Australia
  {
    name: "Bay of Fires",
    description: "Stumpy’s Bay to Bay of Fires Lodge.",
    lat: -40.93777,
    lng: 148.25361,
    category: "nature",
  },
  {
    name: "Great Barrier Reef",
    description: "The Great Barrier Reef is the world's largest coral reef.",
    lat: -17.998522961701028,
    lng: 146.83370745678155,
    category: "nature",
  },

  // Austria
  {
    name: "Hallstatt",
    description: "Hallstatt is a village on Lake Hallstatt's western shore in Austria's mountainous Salzkammergut region.",
    lat: 47.557600303976095,
    lng: 13.64545911243289,
  },

  // Bolivia
  {
    name: "Salar de Uyuni",
    description:
      "Salar de Uyuni, amid the Andes in southwest Bolivia, is the world’s largest salt flat.",
    lat: -20.16324964478777,
    lng: -67.6317594936942,
    category: "nature",
  },

  // Botswana
  {
    name: "Okavango Delta",
    description:
      "The Okavango Delta is a vast inland river delta in northern Botswana. It's known for its sprawling grassy plains, which flood seasonally, becoming a lush animal habitat.",
    lat: -19.628682151586847,
    lng: 22.900244790130884,
    category: "nature",
    img_name: "okavango.jpg",
    img_link: "https://photos.app.goo.gl/m9TapxzjSGvBbNvHA",
    photographer: "me",
    visited: true,
  },

  // Brazil
  {
    name: "Iguazu Falls",
    description: "Large waterfall in the border of Argentina and Brazil.",
    lat: -25.6567959,
    lng: -54.4249245,
    img_name: "iguazu.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/th1Mc4RBhJzFHQsCA",
    visited: true,
    category: "nature",
  },
  {
    name: "Chapada Diamantina",
    description:
      "Rugged, rocky & biodiverse nature preserve known for its waterfalls, caves & former diamond mines.",
    lat: -12.54622966371189,
    lng: -41.69042884729987,
    category: "nature",
  },
  {
    name: "Chapada dos Guimarães",
    description:
      "A viewing deck overlooks this tall, scenic waterfall in Chapada dos Guimarães National Park.",
    lat: -15.405106632100686,
    lng: -55.83118054906799,
    category: "nature",
  },
  {
    name: "Fernando de Noronha",
    description:
      "Fernando de Noronha is a volcanic archipelago about 350 kilometers off Brazil's northeast coast.",
    lat: -3.8554112845505024,
    lng: -32.427750322234026,
    category: "nature",
  },
  {
    name: "Parque Nacional dos Lençóis Maranhenses",
    description:
      "It’s known for its vast desert landscape of tall, white sand dunes and seasonal rainwater lagoons.",
    lat: -2.486253606843343,
    lng: -43.128595120333074,
    category: "nature",
  },
  {
    name: "Pantanal",
    description: "Tropical wetlands home to jaguars, giant otters & native birds, popular for hiking & boat trips",
    lat: -17.66167173401681,
    lng: -57.42970055989979,
    category: "nature",
  },

  // Bhutan
  {
    name: "Paro Taktsang",
    description: "Sacred mountainside monastery with ornately designed temples & art, plus legendary meditation caves.",
    lat: 27.492567586337824,
    lng: 89.36211710089127,
    category: "monastery",
  },

  {
    name: "Punakha Dzong",
    description: "Imposing palace & fortress dating to the 17th century in a serene setting overlooking the river.",
    lat: 27.604243734505143,
    lng: 89.86823615050018,
    category: "fort",
  },

  // Cambodia
  {
    name: "Angkor Wat",
    description:
      "Temple complex surrounded by a wide moat and intricate carvings.",
    lat: 13.4124693,
    lng: 103.8669857,
    category: "monastery",
  },

  // Canada
  {
    name: "Peyto Lake",
    description:
      "Unique glacier-fed waterway in a valley known for its bright turquoise water & scenic beauty.",
    lat: 51.77835376750945,
    lng: -116.53237765205041,
    img_name: "peyto_lake.jpg",
    img_link: "https://photos.app.goo.gl/pqNaYakB8pw1iSht9",
    photographer: "me",
    visited: true,
    category: "nature",
  },

  // Chile
  {
    name: "Easter Island",
    description: "Remote island with some 900 massive stone monuments with giant human heads.",
    lat: -27.08126583499401,
    lng: -109.37133461604651,
    category: "monument",
  },

  {
    name: "Torres del Paine National Park",
    description: "National Park in Patagonia.",
    lat: -50.8604868,
    lng: -73.284805,
    category: "nature",
  },

  // China
  {
    name: "Forbidden City",
    description:
      "Completed in 1420, this palace complex with 980 buildings features a museum with art & artifacts.",
    lat: 39.91645906880579,
    lng: 116.39718479734073,
    img_name: "forbidden_city.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/nkrASFRac9RWN785A",
    visited: true,
  },
  {
    name: "Great Wall of China",
    description:
      "Series of fortifications built to defent agains nomadic groups from the North. This section was built during the Ming Dynasty.",
    lat: 40.44092265997028,
    lng: 116.56548453733048,
    img_name: "great_wall_of_china.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/psHkREotYjhmKGos5",
    visited: true,
  },
  {
    name: "Harbin Ice Festival",
    description:
      "Ice Festival featuring giant colorful sculptures and buildings made of ice.",
    lat: 45.777671,
    lng: 126.6207933,
    img_name: "harbin.jpg",
    photographer: "me",
    img_link: "hhttps://photos.app.goo.gl/uUodovcKS4yxDG227",
    visited: true,
  },
  {
    name: "Kizil Thousand Buddha Caves",
    description: "Extensive complex of ancient carved caves known for Buddhist murals in a variety of styles.",
    lat: 41.78927440243314,
    lng: 82.50799992086527,
    category: "buddhism",
  },
  {
    name: "Longmen Grottoes",
    description: "Vast, UNESCO–listed site with thousands of statues of Buddha carved in caves excavated from cliffs.",
    lat: 34.5983596843618,
    lng: 112.47349736240065,
    category: "buddhism",
  },
  {
    name: "Mogao Caves",
    description: "Complex featuring 492 cave temples with Buddhist artwork, believed to span from 4th-14th centuries.",
    lat: 40.11597653917494,
    lng: 94.83222718539928,
    category: "buddhism",
  },
  {
    name: "Potala Palace",
    description: "Built in the 17th century, this renowned palace complex features hundreds of rooms & shrines.",
    lat: 29.656240082436756,
    lng: 91.11877232048839,
  },
  {
    name: "Shilin Stone Forest in Yunnan",
    description: "Rocky, karst parkland featuring a forest of tall limestone formations & green trees.",
    lat: 24.82363552976476,
    lng: 103.32756741435762,
    category: "nature",
  },
  {
    name: "Summer Palace",
    description:
      "It was an imperial garden in the Qing dynasty.",
    lat: 40.00014663368477,
    lng: 116.27548206298917,
    img_name: "summer_palace.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/v1ZnCx7qfmyFDg1H7",
    visited: true,
  },
  {
    name: "Classical Gardens of Suzhou",
    description: "Built over 1000 years, from the 11th-19th centuries, these gardens define the Chinese style.",
    lat: 31.33455547912024,
    lng: 120.62878325062533,
  },
  {
    name: "Temple of Heaven",
    description:
      "Complex of religious buildings visited by the Emperors of the Ming and Qing dynasties for annual ceremonies of prayer to Heaven for a good harvest.",
    lat: 39.88231198659993,
    lng: 116.40655195975519,
    img_name: "temple_of_heaven.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/DKK5kUFXLCfgoZH3A",
    visited: true,
  },
  {
    name: "Terracota Warriors",
    description: "Lifelike sculptures of terra-cotta warriors built around the 3rd century BC.",
    lat: 34.3842746367123,
    lng: 109.27847034635566,
  },
  {
    name: "Yungang Grottoes",
    description: "Begun in 460 A.D., these massive statues were cut into the rock over the course of 64 years.",
    lat: 40.11172964495947,
    lng: 113.13251317665217,
  },
  {
    name: "Zhangjiajie National Forest Park",
    description: "Tall boulders (seen on Avatar).",
    lat: 29.315305,
    lng: 110.434767,
    category: "nature",
  },

  // Croatia
  {
    name: "Plitvice Lakes National Park",
    description:
      "It's known for a chain of 16 terraced lakes, joined by waterfalls, that extend into a limestone canyon.",
    lat: 44.8653966,
    lng: 15.5820119,
    category: "nature",
  },

  // Egypt
  {
    name: "Abu Simbel Temples",
    description: "Huge 3,000-year-old monument to Ramesses II.",
    lat: 22.337916596915377,
    lng: 31.62672169795875,
    category: "monument",
  },
  {
    name: "Giza Necropolis",
    description: "Includes the Pyramid of Giza, the only remaining of the Seven Wonders of the Ancient World.",
    lat: 29.97747274226785,
    lng:  31.13245258817376,
    img_name: "giza.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/HDAc12RUxrA5ndTm6",
    visited: true,
    category: "monument",
  },
  {
    name: "Luxor Temple",
    description: "Ancient Egyptian temple complex.",
    lat: 25.699502,
    lng: 32.6390509,
    img_name: "luxor.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/o2StDeK5YSPu8RqDA",
    visited: true,
    category: "monument",
  },
  {
    name: "Temple of Edfu",
    description: "One of the best preserved temples in Egypt. From the Ptolemaic period",
    lat: 24.97874299533867,
    lng: 32.873243544663964,
    category: "monument",
  },

  // England
  {
    name: "British Museum",
    description: "One of the largest natural history museums in the world.",
    lat: 51.5193839182863,
    lng: -0.1269542555425667,
    img_name: "rosetta.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/2qFREXqTr8wbKMAv6",
    visited: true,
    category: "museum",
  },
  {
    name: "Roman Baths",
    description: "Constructed between 60-70AD in the first few decades of Roman Britain.",
    lat: 51.38146954984217,
    lng: -2.3595445070296472,
    img_name: "roman_baths.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/Z3Z9yvaZ655PFCxm6",
    visited: true,
    category: "museum",
  },
  {
    name: "White Cliffs of Dover",
    description: "White chalk cliffs rise 350 feet from the sea.",
    lat: 51.13473906458341,
    lng: 1.3572182590023552,
    img_name: "dover_cliffs.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/2qFREXqTr8wbKMAv6",
    visited: true,
    category: "nature",
  },
  // Equador
  {
    name: "Galapagos Island",
    description: "Darwin.",
    lat: -0.82927,
    lng: -90.98206,
    category: "nature",
  },

  // Ethiopia
  {
    name: "Danakil Depression",
    description: "Colorful Hot springs.",
    lat: 14.2417139,
    lng: 40.29459,
    img_name: "danakil_depression.jpg",
    photographer: "Alexander Savin",
    img_link:
      "https://en.wikipedia.org/wiki/Danakil_Depression#/media/File:ET_Afar_asv2018-01_img46_Dallol.jpg",
    category: "nature",
  },
  {
    name: "Rock-Hewn Churches of Lalibela",
    description: "The Rock-hewn Churches of Lalibela are monolithic churches located in the Western Ethiopian Highlands named after King Gebre Mesqel Lalibela, who commissioned the massive building project to recreate the holy city of Jerusalem in his own kingdom.",
    lat: 12.034181950563003,
    lng: 39.04353530422944,
  },

  // France
  {
    name: "Lavender Fields of Provence",
    description: "Lavender Fields of Provence.",
    lat: 43.86508366667945,
    lng: 5.427702093675174,
  },
  {
    name: "Louvre Museum",
    description:
      "The Louvre, or the Louvre Museum, is the world's second-largest art museum.",
    lat: 48.861327592885104,
    lng:  2.3376365452761454,
    category: "museum",
  },
  {
    name: "Mont Saint-Michel",
    description: "Small, inhabited island with 11th-century, Romanesque abbey & the highest tides in Europe.",
    lat: 48.63619093905395,
    lng: -1.5110638484552334,
    category: "fort",
  },
  {
    name: "Palace of Versailles",
    description: "Immense, 18th-century palace with gilded apartments, chandeliered Hall of Mirrors & fountain show.",
    lat: 48.80500618371313,
    lng: 2.1203768639217992,
    category: "fort",
  },

  // Germany
  {
    name: "Neuschwanstein Castle",
    description: "Turreted, 19th-century, hilltop castle built for King Ludwig II.",
    lat: 47.55815322752,
    lng: 10.749821857890307,
    category: "fort",
  },
  {
    name: "Sanssouci Palace",
    description: "Palace built by Frederick the Great with a huge garden (Weinbergterrassen).",
    lat: 52.404261678443625,
    lng: 13.038508133102255,
    category: "fort",
  },

  // Greece
  {
    name: "Acropolis of Athens",
    description:
      "Restored, landmark temple ruins on archaeological site.",
    lat: 37.97156418752821,
    lng: 23.72574013780217,
    img_name: "acropolis.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/j2wXLuJsSnCnckGx6",
    visited: true,
    category: "monument",
  },
  {
    name: "Monasteries of Meteora",
    description:
      "Monasteries built in the 12-13th century on top of tall rock pillars",
    lat: 39.7217044,
    lng: 21.6305896,
    img_name: "meteora.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/2qFREXqTr8wbKMAv6",
    visited: true,
    category: "monastery",
  },
  {
    name: "Navagio Beach",
    description: "Iconic pebble beach between towering cliffs with a shipwreck & turquoise water, reached by boat.",
    lat: 37.85949040887617,
    lng: 20.624864375003003,
    category: "nature",
  },
  {
    name: "Santorini",
    description: "Small Greek Island known for its while buildings",
    lat: 36.4624199,
    lng: 25.3789717,
    img_name: "santorini.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/2qFREXqTr8wbKMAv6",
    visited: true,
  },

  // Guatemala
  {
    name: "Tikal",
    description: "Tikal is an ancient Mayan citadel.",
    lat: 17.22489,
    lng: -89.61103,
    category: "monument",
  },

  // Iceland
  {
    name: "Svartifoss",
    description: "Waterfall and basalt rocks in Skaftafell National Park.",
    lat: 64.0234109,
    lng: -16.9841541,
    category: "nature",
  },

  // India
  {
    name: "Ajanta Caves",
    description: "The Ajanta Caves are approximately 30 rock-cut Buddhist cave monuments which date from the 2nd century BCE to about 480 CE.",
    lat: 20.552181778026124,
    lng: 75.70329502130777,
  },
  {
    name: "Chand Bawri",
    description: "Chand Baori is a stepwell situated in the village of Abhaneri in the Indian state of Rajasthan.",
    lat: 27.007379920230612,
    lng: 76.60643437687226,
  },
  {
    name: "Chittorgarh Fort",
    description: "The Chittor Fort or Chittorgarh is one of the largest fort in India. It is a UNESCO World Heritage Site.",
    lat: 24.897619063948884,
    lng: 74.64477114501925,
    category: "fort",
  },
  {
    name: "Elephanta Caves",
    description: "Elephanta Caves are a UNESCO World Heritage Site and a collection of cave temples predominantly dedicated to the Hindu god Shiva.",
    lat: 18.96386645818726,
    lng: 72.93153649719797,
  },
  {
    name: "Ellora Caves",
    description: "Grand archeological site from the 6th to 10th century featuring religious monuments cut into rock.",
    lat: 20.03011435281772,
    lng: 75.17909161644513,
  },
  {
    name: "Hawa Mahal",
    description: "Beautiful palace of Mughal architecture in Jaipur, India.",
    lat: 26.9251416,
    lng: 75.8211004,
    img_name: "hawa_mahal.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/a8jnk3xNLakdozTW8",
    visited: true,
    category: "monument",
  },
  {
    name: "Kashmir Valley",
    description: "Valley surrounded by the Himalayas",
    lat: 34.08365175624528,
    lng: 74.79720342932576,
  },
  {
    name: "Sri Harmandir Sahib",
    description: "Central worship place for Sikhs around the world, built from white marble overlaid with gold leaf.",
    lat: 31.624759568265535,
    lng: 74.87700819402811,
    category: "sikhism",
  },
  {
    name: "Taj Mahal",
    description: "Beautiful palace of Mughal architecture in Agra, India.",
    lat: 27.1737721,
    lng: 78.0427502,
    img_name: "taj_mahal.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/a8jnk3xNLakdozTW8",
    visited: true,
    category: "monument",
  },
  {
    name: "Kolukkumalai Tea Estate",
    description: "Tea plantations in Munnar, Kerala, India.",
    lat: 10.075677536514016,
    lng: 77.22178397931653,
  },
  {
    name: "Meenakshi Amman Temple",
    description: "Ancient, walled, Hindu place of worship with sacred water tank and colourful, sculpted gate towers.",
    lat: 9.920519070884145,
    lng: 78.119663666074,
  },
  {
    name: "Venkateswara Temple",
    description: "Venkateswara Temple is a Hindu temple in Tirumala and dedicated to Venkateswara, a form of Vishnu. It's believed to have been constructed starting in 300 AD. It is the richest temple in the world in terms of donations received and wealth. It was a very unique experience to join the crowds through the temple. Photos not allowed inside.",
    lat: 13.68325,
    lng: 79.347194,
    img_name: "tirumala.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/fKEguwmyLAZLno7h7",
    visited: true,
  },

  // Indonesia
  {
    name: "Borobudur",
    description:
      "Dating to the 9th century, this site is renowned as the largest Buddhist temple in the world.",
    lat: -7.607342627039068,
    lng: 110.2037284446244,
    category: "buddhism",
  },
  {
    name: "Komodo National Park",
    description:
      "Expansive national park that aims to protect endangered Komodo dragons & other wildlife.",
    lat: -8.527647033553302,
    lng: 119.48323933372177,
  },
  {
    name: "Ratenggaro Village",
    description:
      "Traditional Village with iconic high roofs.",
    lat: -9.62720973623224,
    lng: 119.0031081065439,
  },
  {
    name: "Tumpak Sewu Waterfalls",
    description:
      "The waterfall is overshadowed by Semeru, an active volcano and the highest mountain in Java.",
    lat: -8.230778637200986,
    lng: 112.91777188048432,
    category: "nature",
  },

  // Iran
  {
    name: "Ancient Windmills of Nashtifan",
    description: "1,000 old windmills.",
    lat: 34.43280424454187,
    lng: 60.175524191400186,
    category: "monument",
  },
  {
    name: "Bam Citadel",
    description: "The Arg-e Bam is the largest adobe building in the world.",
    lat: 29.115176273815155,
    lng: 58.369317332187286,
    category: "monument",
  },
  {
    name: "Persepolis",
    description: "Ruins of a 518 BC Achaemenid empire capital.",
    lat: 29.933267569582167,
    lng: 52.886444738853385,
    category: "monument",
  },
  {
    name: "Shushtar Historical Hydraulic System",
    description: "Complex irrigation system & UNESCO site with water mills, dams, tunnels & canals.",
    lat: 32.044218764946585,
    lng: 48.85846733320769,
    category: "monument",
  },

  // Iraq
  {
    name: "Ziggurat of Ur",
    description: "Monumental place of worship built for a Bronze Age Sumerian king, often restored over 4,000 years.",
    lat: 30.96649516884495,
    lng: 46.10404623212625,
    category: "monument",
  },

  // Ireland
  {
    name: "Cliffs of Moher",
    description: "Cliffs in Ireland.",
    lat: 52.97278178463433,
    lng: -9.430060029427306,
    category: "nature",
  },

  // Israel
  {
    name: "Dead Sea",
    description: "Dead sea is one of the world's saltiest bodies of water, which makes swimming similar to floating. Its shores are the lowest land-based elevation on Earth.",
    lat: 31.456842637032324,
    lng:35.40082030259397,
     img_name: "dead_sea.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/LbEBtaP3iwwwPyK88",
    visited: true,
    category: "nature",
  },
  {
    name: "Old Jerusalem",
    description: "So much history packed here. Old Jerusalem includes the Armenian, Christian, Jewish and Muslim quarters, and it's a sacred place to all these western religions. It's a UNESCO World Heritage Site.",
    lat: 31.77723810641537,
    lng:35.23517136655842,
    img_name: "old_jerusalem.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/EFdu9K6pEkMW986W7",
    visited: true,
  },

  // Italy
  {
    name: "Cinque Terre",
    description:
      "Cinque Terre is a string of centuries-old seaside villages on the rugged Italian Riviera coastline.",
    lat: 44.1237586,
    lng: 9.6738363,
  },
  {
    name: "Duomo di Milano",
    description: "One of the largest cathedrals in the world, this iconic masterpiece took over 600 years to complete.",
    lat: 45.46431327475497,
    lng: 9.19193965680113,
  },
  {
    name: "Colosseum",
    description:
      "Largest ancient amphitheatre ever built, and is still the largest standing amphitheater in the world today.",
    lat: 41.8902102,
    lng: 12.4900422,
    category: "monument",
  },
  {
    name: "Tower of Pisa",
    description:
      "Elaborately adorned 14th-century tower (56 meters at its tallest point) with a world-famous lean.",
    lat: 43.72307237401429,
    lng: 10.396613548653278,
    category: "monument",
  },
  {
    name: "Venice",
    description:
      "Venice, the capital of northern Italy’s Veneto region, is built on more than 100 small islands in a lagoon in the Adriatic Sea. It has no roads, just canals",
    lat: 45.43440028570622,
    lng: 12.338469167604442,
  },
  {
    name: "The Trulli of Alberobello",
    description: "The trulli , limestone dwellings found in the southern region of Puglia, are remarkable examples of drywall (mortarless) construction, a prehistoric building technique still in use in this region.",
    lat: 40.783138320116066,
    lng: 17.239711151841274,
  },

  // Japan
  {
    name: "Fushimi Inari Shrine",
    description: "Known for the vast number of orange Torii gates",
    lat: 34.967340614218074,
    lng: 135.77263945495233,
    img_name: "fushimi_inari.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/LbEBtaP3iwwwPyK88",
    visited: true,
  },
  {
    name: "Himeji Castle",
    description: "Iconic circa-1613 castle known for a white facade, plus towers, moats, passageways & cherry trees.",
    lat: 34.83963388102556,
    lng: 134.69393689130462,
    img_name: "himeji.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/GPRJLFJBfcRdmyUA9",
    visited: true,
  },
  {
    name: "Hiroshima",
    description: "One of the two sites of the atomic bomb during WWII. The Atomic Bomb Dome is one of the few that survived partially.",
    lat: 34.39638540365954,
    lng: 132.45345523952764,
    img_name: "hiroshima.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/2SjdmDWExx1b94Vs6",
    visited: true,
  },
  {
    name: "Shirakawa-go",
    description:
      "Historic Villages of Shirakawa-gō. One of Japan's UNESCO World Heritage Sites",
    lat: 36.2302784,
    lng: 136.7303166,
    img_name: "shirakawa.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/VQLqoBprw8jnPbqVA",
    visited: true,
  },
  {
    name: "Ashikaga Flower Park",
    description:
      "Charming flower gardens offering 8 seasonal thematic displays & a spring wisteria festival.",
    lat: 36.315072480873724,
    lng: 139.52010803165786,
  },
  {
    name: "Tokyo National Museum",
    description:
      "Museum complex devoted to the art & antiquities of Japan, as well as other Asian countries..",
    lat: 35.7186819141621,
    lng: 139.77622731392006,
    visited: true,
    img_name: "tokyo_museum.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/FmpXGpv154GrmF916",
    category: "museum",
  },

  // Jordan
  {
    name: "Petra",
    description:
      "Petra is an archaeological site in Jordan. Dating to around 300 B.C., it was the capital of the Nabatean Kingdom.",
    lat: 30.3284544,
    lng: 35.4443622,
    img_name: "petra.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/C7rXHX5kpWHCfnbL6",
    visited: true,
    category: "monument",
  },

  // Kazakhstan
  {
    name: "Kaindy Lake",
    description:
      "Lake Kaindy is a 400-meter-long lake located in Kazakhstan.",
    lat: 42.98478876084527,
    lng: 78.46588932854154,
    category: "nature",
  },

  // Lybia
  {
    name: "Old Town of Ghadamès",
    description:
      "Ghadames is an oasis Berber town, known for its architecture",
    lat: 30.132452278898395,
    lng: 9.49724608232273,
    category: "monument",
  },

  // Madagascar
  {
    name: "Avenue of the Baobabs",
    description:
      "Famed group of towering baobab trees lining the dirt road from Morondava to Belon'i Tsiribihina.",
    lat: -20.2560794,
    lng: 44.1756571,
    category: "nature",
  },
  {
    name: "Tsingy de Bemaraha Strict Nature Reserve",
    description:
      "A UNESCO World Heritage Site, this park is known for its dramatic limestone geological formations.",
    lat: -18.4368781,
    lng: 44.7381432,
    category: "nature",
  },

  // Malaysia
  {
    name: "Batu Caves",
    description:
      "Limestone caves at the top of steep steps housing Hindu temples & shrines, plus a huge deity statue.",
    lat: 3.2602148,
    lng: 101.5868289,
  },

  // Mali
  {
    name: "Bandiagara Escarpment",
    description:
      "First inhabited by the Telem people between the 11th and 16th centuries, it has since then been inhabited by the Dogon people.",
    lat: 14.434378914414124,
    lng:  -3.3268840475372663,
  },
  {
    name: "Timbuktu",
    description:
      "Timbuktu started out as a seasonal settlement and became a permanent settlement early in the 12th century. Known for its ",
    lat: 16.77212235879037,
    lng: -3.010168226183718,
  },

  // Mexico
  {
    name: "Basaltic Prisms of Santa María Regla",
    description:
      "Tall columnar joints of basalt rock with a waterfall.",
    lat: 20.2359047716646,
    lng: -98.56230648263956,
    category: "nature",
  },
  {
    name: "Cascadas de Agua Azul",
    description:
      "Series of waterfalls found on the Xanil River.",
    lat: 17.258004335468748,
    lng: -92.11505243778028,
    category: "nature",
  },
  {
    name: "Gran Cenote",
    description:
      "One of multiple cenotes (underground sinkholes) in the Yucatan Peninsula.",
    lat: 20.246823368379435,
    lng: -87.46415808600378,
    category: "nature",
  },
  {
    name: "Chichén-Itzá",
    description:
      "Archaeological site with excavated ruins of the large Maya city.",
    lat: 20.680334,
    lng: -88.5707365,
    category: "monument",
  },
  {
    name: "El Tajin",
    description:
      "Hosts the Pyramid of the Niches, a masterpiece of ancient Mexican and American architecture.",
    lat: 20.443268954486545,
    lng: -97.37813941920652,
    category: "monument",
  },
  {
    name: "Guachimontones",
    description:
      "Large circular pyramids are the highlight of this unique archaeological site dating to 300 BCE.",
    lat: 20.69785233998074,
    lng: -103.83396882679372,
    category: "monument",
  },
  {
    name: "Hierve el Agua",
    description:
      "Ancient geological site featuring towering, waterfall-like rock formations, pools & springs.",
    lat: 16.865753242292385,
    lng: -96.27599567087927,
    category: "nature",
  },
  {
    name: "Museo Nacional de Antropología",
    description:
      "Anthropology museum showcasing artifacts from as far back as the ancient Maya civilization.",
    lat: 19.426594088199124,
    lng: -99.18626292196433,
    category: "museum",
  },
  {
    name: "Palenque National Park",
    description:
      "Palenque, also anciently known as Lakamha, was a Maya city state in southern Mexico that flourished in the 7th century.",
    lat: 17.485512166526405,
    lng: -92.04572560297886,
    category: "monument",
  },

  // Myanmar
  {
    name: "Old Bagan",
    description:
      "Bagan is an ancient city and a UNESCO World Heritage Site in the Mandalay Region of Myanmar",
    lat: 21.171739824825504,
    lng: 94.85881533957875,
    category: "monument",
  },

  // Morocco
  {
    name: "Aït Benhaddou",
    description: "Aït Benhaddou is a historic ighrem or ksar along the former caravan route between the Sahara and Marrakech in present-day Morocco.",
    lat: 31.047051258288825,
    lng:  -7.131931145052519,
    category: "monument",
  },
  {
    name: "Chefchaouen",
    description: "Blue City in Morocco.",
    lat: 35.1698002,
    lng: -5.2865695,
  },

  // Namibia
  {
    name: "Deadvlei",
    description:
      "Deadvlei is a white clay pan located near the more famous salt pan of Sossusvlei, inside the Namib-Naukluft Park in Namibia.",
    lat: -24.7592732,
    lng: 15.2923894,
    category: "nature",
    img_name: "deadvlei.jpg",
    img_link: "https://photos.app.goo.gl/JKtCM9XcmAiU5DsE8",
    photographer: "me",
    visited: true,
  },

  {
    name: "Etosha National Park",
    description:
      "Game reserve in Namibia. In here, it's possible to see many big mammals at waterholes such as elephants, giraffes and black rhinoceros. The waterhole in Okaukuejo is particularly good.",
    lat: -19.167069330285752,
    lng: 15.91759313453436,
    category: "nature",
    img_name: "etosha.jpg",
    img_link: "https://photos.app.goo.gl/4xr7Bi5QPHKW19YJ6",
    photographer: "me",
    visited: true,
  },

  // Nepal
  {
    name: "Mount Everest Base Camp",
    description: "n/a",
    lat: 27.9343193,
    lng: 86.7818523,
    category: "nature",
  },

  // Netherlands
  {
    name: "Keukenhof",
    description:
      "Keukenhof, also known as the Garden of Europe, is one of the world's largest flower gardens, situated in the town of Lisse, in the Netherlands.",
    lat: 52.2697282,
    lng: 4.5469674,
  },

  // New Zealand
  {
    name: "Mt Cook / Aoraki National Park",
    description: "Hikes with views of Mt Cook / Aoraki",
    img_name: "aoraki.jpg",
    photographer: "me",
    lat: -43.72031701302545,
    lng: 170.06440888992094,
    category: "nature",
    img_link: "https://photos.app.goo.gl/AVszNeXVdj59mUhp8",
    visited: true,
  },
  {
    name: "Piopiotahi",
    description:
      "Milford Sound is a fiord in the southwest of New Zealand’s South Island. It’s known for towering Mitre Peak, plus rainforests and waterfalls like Stirling and Bowen falls, which plummet down its sheer sides.",
    img_name: "milford.jpg",
    photographer: "me",
    lat: -44.63507813222853,
    lng: 167.89796460430344,
    category: "nature",
    img_link: "https://photos.app.goo.gl/5WGQieugPzLBpMRv8",
    visited: true,
  },
  {
    name: "Punakaiki Pancake Rocks",
    description:
      "Pancake Rocks are a heavily eroded limestone area where the sea bursts through a number of vertical blowholes during high tides.",
    img_name: "punakaiki.jpg",
    photographer: "me",
    lat:-42.114105127032076,
    lng: 171.32627617034458,
    category: "nature",
    img_link: "https://photos.app.goo.gl/DHepHFJd8v4SeRmj8",
    visited: true,
  },

  // Norway
  {
    name: "Geirangerfjord",
    description:
      "Iconic fjord offering snowy mountain peaks, lush plant life, cascading falls & spectacular views.",
    lat: 62.10331827445209,
    lng: 7.095297294161887,
    category: "nature",
  },
  {
    name: "Preikestolen",
    description:
      "Popular mountainous hike to a famed 604-metre cliff with a flat top, offering panoramic views.",
    lat: 58.9868234,
    lng: 6.186655,
    category: "nature",
  },

  // Peru
  {
    name: "Machu Picchu",
    description: "Ancient Inca Site.",
    lat: -13.1631412,
    lng: -72.5471516,
    category: "monument",
  },
  {
    name: "Titicaca",
    description: "Largest lake in South America between Bolivia and Peru.",
    lat: -15.4885423,
    lng: -69.3133206,
    category: "nature",
  },

  // Philippines
  {
    name: "Chocolate Hills Complex",
    description:
      "Popular observation area known for its panoramic views of its hilly, tree-filled surrounds.",
    lat: 9.80489371215848,
    lng: 124.16906501364669,
    category: "nature",
  },

  // Portugal
  {
    name: "Seven Hanging Valleys in the Algarve",
    description:
      "Trail along the Mediterranean coast in the Algarve. View of interesting limestone rocks formations including the Benagil cave.",
    img_name: "algarve.jpg",
    photographer: "me",
    lat: 37.09085349876126,
    lng: -8.414169190725532,
    category: "nature",
    img_link: "https://photos.app.goo.gl/tnMzteSDQuAZNVtm9",
    visited: true,
  },

  {
    name: "National Tile Museum",
    description:
      "Museum showcasing azulejos from Portugal.",
    img_name: "azulejo.jpg",
    photographer: "me",
    lat: 38.72519799051279,
    lng: -9.113517533079035,
    category: "museum",
    img_link: "https://photos.app.goo.gl/Jp5w1jJhvhJHBq2JA",
    visited: true,
  },


  // Russia
  {
    name: "St. Basil's Cathedral",
    description:
      "Multicolored domes top this 16th-century former cathedral that now contains a museum of the church.",
    lat: 55.752740264051184,
    lng: 37.62297951594253,
    category: "church",
  },

  // Saudi Arabi
  {
    name: "Kaaba",
    description:
      "The Kaaba a building at the center of Islam's most important mosque, the Masjid al-Haram. It is the most sacred site in Islam",
    lat: 21.42263769517834,
    lng: 39.82619172029739,
  },

  // Scotland
  {
    name: "Fingal's Cave",
    description: "Cave known for its resounding acoustics & abstract rock formations.",
    lat: 56.43145163449158,
    lng: -6.341378939035621,
    category: "nature",
  },
  {
    name: "The Storr",
    description: "Rock formation in Northern Scotland.",
    lat: 57.4959496,
    lng: -6.1972455,
    category: "nature",
  },

  // Seychelles
  {
    name: "Vallée de Mai National Park",
    description: "Lush, 19.5-hectare preserve with an ancient forest of Coco de Mer palm trees & many endemic animals.",
    lat: -4.327765594023093,
    lng: 55.74042821862747,
    category: "nature",
  },

  // South Africa
  {
    name: "Kirstenbosch National Botanical Garden",
    description: "Hillside garden & nature reserve with rare plant species, walking trails & mountain views",
    lat: -33.987287536530026,
    lng: 18.43306531479488,
    category: "museum",
    img_name: "kirstenbosch.jpg",
    img_link: "https://photos.app.goo.gl/yeFEdj27JruZasqi6",
    photographer: "me",
    visited: true,
  },

  // Spain
  {
    name: "Alhambra",
    description:
      "One of the most famous monuments of Islamic architecture and one of the best-preserved palaces of the historic Islamic world.",
    lat: 37.176773008467656,
    lng: -3.5897565660552555,
    category: "fort",
    photographer: "me",
    img_name: "alhambra.jpg",
    img_link: "https://photos.app.goo.gl/44fKATY2ht2q5W8Q7",
    visited: true,
  },
  {
    name: "Castillo de Coca",
    description:
      "The castle was constructed in the 15th century and has been considered to be one of the best examples of Spanish Mudejar brickwork which incorporates Moorish Muslim design and construction with Gothic architecture.",
    lat: 41.21592421806536,
    lng: -4.5254420841882705,
    category: "fort",
  },
  {
    name: "El Caminito Del Rey",
    description:
      "El Caminito del Rey is a walkway pinned along the steep walls of a narrow gorge in El Chorro.",
    lat: 36.93270958448457,
    lng: -4.789922155261637,
    category: "fort",
    img_name: "el_caminito.jpg",
    img_link: "https://photos.app.goo.gl/W4ZkUN4QsTgxc4yu9",
    photographer: "me",
    visited: true,
  },
  {
    name: "Sagrada Família",
    description: "Roman Catholic Church, designed by the Spanish architect Antoni Gaudí",
    lat: 41.403773864564776,
    lng: 2.1743782194785273,
    category: "church",
  },
  {
    name: "Parc Güell",
    description: "Mosaic-covered buildings, steps & sculptures in verdant park with the Gaudi museum.",
    lat: 41.41432102633736,
    lng: 2.152587851575814,
  },
  {
    name: "Ronda",
    description: "Beautiful city atop of a mountain.",
    lat: 36.7462,
    lng: -5.16122,
    img_name: "ronda.jpg",
    img_link: "https://photos.app.goo.gl/vdYdN18SW34byv8bA",
    photographer: "me",
    visited: true,
  },
  {
    name: "Mosque-Cathedral of Córdoba",
    description:
      "The Mosque–Cathedral of Córdoba, officially known by its ecclesiastical name, the Cathedral of Our Lady of the Assumption is the cathedral of the Roman Catholic Diocese of Córdoba dedicated to the Assumption of Mary and located in the Spanish region of Andalusia.",
    lat: 37.8789,
    lng: -4.77938,
    img_name: "mosque_cathedral.jpg",
    img_link: "https://photos.app.goo.gl/nkaeBrpXny6D7sMN6",
    photographer: "me",
    visited: true,
  },
  {
    name: "Setenil de las Bodeguitas",
    description:
      "A small town where many buildings were constructed under giant boulders.",
    lat: 36.86212982542259,
    lng: -5.1791164384202455,
    img_name: "setenil.jpg",
    img_link: "https://photos.app.goo.gl/LeF67uEs8rSCvHna6",
    photographer: "me",
    visited: true,
  },

  // Sudan
  {
    name: "Pyramids of Meroë",
    description:
      "Meroë was an ancient city, the capital of the Kingdom of Kush for several centuries from around 590 BC.",
    lat: 16.93380345385708,
    lng: 33.728611796002646,
    category: "monument",
  },

  // Sweden
  {
    name: "Kungsleden",
    description:
      "Kungsleden is a hiking trail in northern Sweden, approximately 440 kilometres long, between Abisko in the north and Hemavan in the south. It passes through, near the southern end, the Vindelfjällen Nature Reserve, one of the largest protected areas in Europe.",
    lat: 68.36168,
    lng: 18.7234,
    category: "nature",
  },
  {
    name: "Laitaure Delta",
    description:
      "The Rapa delta is considered to be the most beautiful river delta in Scandinavia. It can be seen from above from the Skierfeklippan peak",
    lat: 67.16749163371166,
    lng: 18.212099916970978,
    category: "nature",
  },

  // Taiwan
  {
    name: "Fo Guang Shan Buddha Museum",
    description:
      "Huge museum complex with one of the 10 tallest Buddha statue in the world.",
    lat: 22.75736361287764,
    lng: 120.44030743786956,
    img_name: "fo_guang_shan.jpg",
    photographer: "me",
    img_link: "https://photos.app.goo.gl/fAUHGvfEoqPAuYzV6",
    category: "nature",
    visited: true,
  },

  // Tanzania
  {
    name: "Mount Kilimanjaro",
    description:
      "It's a dormant volcano and the highest mountain in Africa.",
    lat: -3.066666958726656,
    lng: 37.35533880304339,
    category: "nature",
  },
  {
    name: "Serengeti National Park",
    description:
      "Vast nature reserve best known for its annual wildebeest migration, with lions, elephants & rhino.",
    lat: -2.333880200584234,
    lng: 34.83305583000156,
    category: "nature",
  },

  // Togo
  {
    name: "Koutammakou",
    description:
      "The area features traditional mud tower-houses which remain the preferred style of living. The traditional mud houses are known as a national symbol of Togo.",
    lat: 10.0681449455914,
    lng: 1.1354395959874588,
  },

  // Tunisia
  {
    name: "Carthage Ruins",
    description:
      "Carthage was one of the most afluent cities of the classical world and the capital of the Punic empire. It has been destroyed many times over but some of its ruins remains.",
    lat: 36.85378309256145,
    lng: 10.324563704538333,
    category: "monument",
  },

  // Turkey
  {
    name: "Archaeological Museum",
    description:
      "Museum showing Turkey's rich archaeological heritage",
    lat: 41.01045960031307,
    lng: 28.983437611651677,
    category: "museum",
  },
  {
    name: "Cappadocia",
    description:
      "Known for its distinctive “fairy chimneys,” tall, cone-shaped rock formations clustered in Monks Valley, Göreme and elsewhere",
    lat: 38.649597784097764,
    lng: 34.83383186504822,
    category: "nature",
  },
  {
    name: "Ephesus",
    description:
      "Ancient Greek city which includes the Library of Celsus and the site of the Temple of Artemis ruins.",
    lat: 37.939620566766585,
    lng: 27.340722058573096,
  },
  {
    name: "Derinkuyu Underground City",
    description:
      "The Derinkuyu underground city is an ancient multi-level underground city.",
    lat: 38.37375643390762,
    lng: 34.73505487666728,
  },
  {
    name: "Pamukkale",
    description: "Cotton Castle (Turkish). Thermal Springs",
    lat: 37.92337084547643,
    lng: 29.131354362511228,
    category: "nature",
  },

  // Turkmenistan
  {
    name: "Darvaza Gas Crater",
    description:
      "Natural gas field with a collapsed crater thought to have been burning continuously since 1971.",
    lat: 40.2526031,
    lng: 58.4397004,
    category: "nature",
  },

  // UAE
  {
    name: "Sheikh Zayed Grand Mosque",
    description: "The largest mosque in Abu Dhabi.",
    lat: 24.423573631874092,
    lng: 54.47371836430741,
  },

  // Uganda
  {
    name: "Bwindi Impenetrable National Park",
    description: "Park on mountainous, densely forested terrain, best known for viewing rare mountain gorillas.",
    lat: -1.0352936376733395,
    lng: 29.691351647005504,
  },

  // Uzbekistan
  {
    name: "Registan",
    description:
      "The Registan was the heart of the ancient city of Samarkand of the Timurid Empire, now in Uzbekistan.",
    lat: 39.65487833264421,
    lng: 66.9757633211604,
  },

  // Venezuela
  {
    name: "Mount Roraima",
    description:
      "Mount Roraima is the highest of the Pakaraima chain of tepuis or plateaux in South America.",
    lat: 5.14333,
    lng: -60.7625,
    category: "nature",
  },

  // Vietnam
  {
    name: "Chăm Ruins in Mỹ Sơn",
    description:
      "The Chăm civilization (Champa) was a kingdom in what is now central Vietnam from the 2nd to the 19th century. Influenced by Indian culture, the Chăm people practiced Hinduism and later Buddhism, constructing impressive temple complexes like Mỹ Sơn.",
    lat: 15.764468047752997,
    lng: 108.12527866492422,
    img_name: "cham.jpg",
    img_link: "https://photos.app.goo.gl/z5SSjaQMvv4hZ3Af7",
    photographer: "me",
    visited: true,
  },
  {
    name: "Hạ Long Bay",
    description: "Multiple tree-covered limestone islands.",
    lat: 20.9361865,
    lng: 107.1593762,
    category: "nature",
    img_name: "ha_long.jpg",
    img_link: "https://photos.app.goo.gl/myGJsDfRqL98CwHVA",
    photographer: "me",
    visited: true,
  },
  {
    name: "Huế",
    description:
      "Huế is a city in central Vietnam that was the seat of Nguyen Dynasty emperors and the national capital from 1802 to 1945. Photo: Mausoleum of Emperor Khai Dinh",
    lat: 16.46800192863555,
    lng: 107.57920881748073,
    img_name: "hue.jpg",
    img_link: "https://photos.app.goo.gl/T5fyoCveapARhASf6",
    photographer: "me",
    visited: true,
  },
  {
    name: "Mù Cang Chải",
    description:
      "Mù Cang Chải is a rural district of Yên Bái Province, in the Northwest region of Vietnam.",
    lat: 21.795010848388312,
    lng: 104.1515230568566,
  },

  // Yemen
  {
    name: "Shibam",
    description: "Manhattan of the Desert.",
    lat: 15.92152364651126,
    lng: 48.63633519304204,
  },
  {
    name: "Socotra Island",
    description: "Island with the unique dragon's blood tree.",
    lat: 12.511573363989033,
    lng: 53.83185149889994,
    category: "nature",
  },

  // Zimbabwe
  {
    name: "Great Zimbabwe",
    description: "Great Zimbabwe is a medieval city in the south-eastern hills of the modern country of Zimbabwe.",
    lat: -20.165199793009425,
    lng: 30.914683760932235,
    category: "monument",
  },
  {
    name: "Victoria Falls",
    description: "Waterfall on the Zambezi river, on the border of Zambia and Zimbabwe, is one of the world's largest waterfalls.",
    lat: -17.92342592464889,
    lng: 25.853602928640754,
    category: "nature",
    img_name: "vic_falls.jpg",
    img_link: "https://photos.app.goo.gl/ozQuLk7VtP8FsaoaA",
    photographer: "me",
    visited: true,
  }
];
