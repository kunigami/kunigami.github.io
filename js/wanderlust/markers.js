// General notes:
//   - Resize images to be 300px on the largest dimension
const markers = [
  // United States
  // -- Arizona
  {
    name: "Cactus Forest",
    description: "Cacti from the Saguaro National Park",
    lat: 32.23041339658249,
    lng: -111.14510525157992,
    img_name: "saguaro.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/50803431808/",
    visited: true,
  },
  {
    name: "Chiricahua National Monument",
    description: "Amazing rock formations in Southeastern Arizona",
    lat: 32.0135714,
    lng: -109.3422953,
    img_name: "chiricahua.png",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/50788433627/",
    visited: true,
  },
  {
    name: "Antelope Canyon",
    description:
      "Antelope Canyon is a slot canyon in the American Southwest, on Navajo land east of Page, Arizona.",
    lat: 36.86470241618197,
    lng: -111.37357693817302,
  },
  {
    name: "Grand Canyon",
    description:
      "One of the most famous canyons in the world, carved by the Colorado river.",
    lat: 36.06714174570464,
    lng: -112.11638907022639,
    img_name: "grand_canyon.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/51334389876/",
    visited: true,
  },
  {
    name: "Havasu Falls",
    description:
      "Havasu Falls is a waterfall of Havasu Creek, located in the Grand Canyon, Arizona, United States. It is within Havasupai tribal lands.",
    lat: 36.25549664057389,
    lng: -112.6978814550031,
  },
  {
    name: "Horseshoe Bend",
    description:
      "Horseshoe Bend is a horseshoe-shaped incised meander of the Colorado River located near the town of Page, Arizona, United States.",
    lat: 36.88032695559509,
    lng: -111.51014454598656,
  },
  {
    name: "Monument Valley",
    description:
      "Famous natural area on the Navajo Nation Reservation features towering red sandstone buttes.",
    lat: 36.99818269582502,
    lng: -110.09847885332755,
  },
  // -- California
  {
    name: "Ancient Bristlecone Pine Forest",
    description: "Oldest Trees in the world",
    lat: 37.38079104393285,
    lng: -118.16166570419409,
    img_name: "bristlecone.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/50812863502/",
    visited: true,
  },
  {
    name: "Cholla Garden",
    description: "Garden of the cactus plant known as Teddy Bear Cholla in Joshua Tree National Park",
    lat: 33.925661614092725,
    lng: -115.92893906366216,
    img_name: "cholla.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/51033477057",
    visited: true,
  },
  {
    name: "Cinder Cone at Lassen National Park",
    description:
      "The cone was built to a height of 230 m above the surrounding area. Then, like many cinder cones, it was snuffed out when several basalt lava flows erupted from its base",
    lat: 40.547385,
    lng: -121.319973,
    img_name: "cinder_cone.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/51068149218/",
    visited: true,
  },
  {
    name: "Forty-Nine Palm Oasis",
    description: "Palm Oasis in the middle of the desert in Joshua Tree National Parl",
    lat: 34.10556608298197,
    lng: -116.1047898389797,
    img_name: "palm_oasis.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/51068149408",
    visited: true,
  },
  {
    name: "Red Rock Canyon State Park",
    description: "Joshua Trees and Red Rock formations",
    lat: 35.3975446,
    lng: -117.9541879,
    img_name: "red_rock_canyon.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/50787619163/",
    visited: true,
  },
  {
    name: "Redwoods National Park",
    description: "Old growth Redwood forest. Home of the tallest trees in the world",
    lat: 41.40104620211404,
    lng: -124.06586779793305,
    img_name: "redwoods.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/51033477057",
    visited: true,
  },
  // -- Colorado
  {
    name: "Cliff Palace in Mesa Verde National Park",
    description:
      "Cliff dwelling built by Ancestral Puebloans from 1190 to 1260.",
    lat: 37.16779555670772,
    lng: -108.47332428961647,
  },
  // -- Hawaii
  {
    name: "Kalalau Valley",
    description:
      "The Kalalau Valley is located on the northwest side of the island of Kauaʻi in the state of Hawaii.",
    lat: 22.1799987,
    lng: -159.6711207,
  },
  // -- New Mexico
  {
    name: "Chaco Culture National Historical Park",
    description:
      "Ruins from Puebloan people.",
    lat: 36.05536703154945,
    lng: -107.95712276204746,
  },
  {
    name: "White Sands National Park",
    description:
      "Landscape filled with gypsum sand dunes.",
    lat: 32.78778371883236,
    lng: -106.32397942084717,
    img_name: "white_sands.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/51334597028/",
    visited: true,
  },
  // Texas
  {
    name: "Santa Elena Canyon",
    description: "Giant walls surrounding the Rio Grande, in the border of the US and Mexico",
    lat: 29.172282302934672,
    lng: -103.60246473625729,
    img_name: "santa_elena_canyon.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/51335412760/",
    visited: true,
  },
  // -- Utah
  {
    name: "Bryce Canyon",
    description:
      "Crimson-colored hoodoos, which are spire-shaped rock formations",
    lat: 37.566307868258306,
    lng: -112.21969399706641,
    img_name: "bryce.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/50807912907/",
    visited: true,
  },
  {
    name: "The Wave",
    description: "Unique Rock Formation in Southern Utah",
    lat: 36.99593,
    lng: -112.00625,
  },
  // -- Washington
  {
    name: "Diablo Lake",
    description: "Cyan blue lake in North Cascades National Park.",
    lat: 48.7142378,
    lng: -121.1355309,
    img_name: "diablo_lake.png",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/50804258467/",
    visited: true,
  },
  // -- Wyoming
  {
    name: "Yellowstone National Park",
    description: "Colorful hot springs and geysers",
    lat: 44.525121,
    lng: -110.8403384,
    img_name: "yellowstone.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/50788723171/",
    visited: true,
  },

  // Algeria
  {
    name: "Timgad",
    description: "Timgad was a Roman city in the Aurès Mountains of Algeria. It was founded by the Roman Emperor Trajan around CE 100.",
    lat: 35.49726639943841,
    lng: 6.469364172911276,
  },

  // Australia
  {
    name: "Bay of Fires",
    description: "Stumpy’s Bay to Bay of Fires Lodge.",
    lat: -40.93777,
    lng: 148.25361,
  },
  {
    name: "Great Barrier Reef",
    description: "The Great Barrier Reef is the world's largest coral reef.",
    lat: -17.998522961701028,
    lng: 146.83370745678155,
  },

  // Bolivia
  {
    name: "Salar de Uyuni",
    description:
      "Salar de Uyuni, amid the Andes in southwest Bolivia, is the world’s largest salt flat.",
    lat: -20.16324964478777,
    lng: -67.6317594936942,
  },

  // Botswana
  {
    name: "Okavango Delta",
    description:
      "The Okavango Delta is a vast inland river delta in northern Botswana. It's known for its sprawling grassy plains, which flood seasonally, becoming a lush animal habitat.",
    lat: -19.628682151586847,
    lng: 22.900244790130884,
  },

  // Brazil
  {
    name: "Iguazu Falls",
    description: "Large waterfall in the border of Argentina and Brazil.",
    lat: -25.6567959,
    lng: -54.4249245,
    img_name: "iguazu.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/50788055268",
    visited: true,
  },
  {
    name: "Chapada Diamantina",
    description:
      "Rugged, rocky & biodiverse nature preserve known for its waterfalls, caves & former diamond mines.",
    lat: -12.54622966371189,
    lng: -41.69042884729987,
  },
  {
    name: "Chapada dos Guimarães",
    description:
      "A viewing deck overlooks this tall, scenic waterfall in Chapada dos Guimarães National Park.",
    lat: -15.405106632100686,
    lng: -55.83118054906799,
  },
  {
    name: "Fernando de Noronha",
    description:
      "Fernando de Noronha is a volcanic archipelago about 350 kilometers off Brazil's northeast coast.",
    lat: -3.8554112845505024,
    lng: -32.427750322234026,
  },
  {
    name: "Parque Nacional dos Lençóis Maranhenses",
    description:
      "It’s known for its vast desert landscape of tall, white sand dunes and seasonal rainwater lagoons.",
    lat: -2.486253606843343,
    lng: -43.128595120333074,
  },

  // Austria
  {
    name: "Hallstatt",
    description: "Hallstatt is a village on Lake Hallstatt's western shore in Austria's mountainous Salzkammergut region.",
    lat: 47.557600303976095,
    lng: 13.64545911243289,
  },

  // Bhutan
  {
    name: "Paro Taktsang",
    description: "Sacred mountainside monastery with ornately designed temples & art, plus legendary meditation caves.",
    lat: 27.492567586337824,
    lng: 89.36211710089127,
  },

  {
    name: "Punakha Dzong",
    description: "Imposing palace & fortress dating to the 17th century in a serene setting overlooking the river.",
    lat: 27.604243734505143,
    lng: 89.86823615050018,
  },

  // Cambodia
  {
    name: "Angkor Wat",
    description:
      "Temple complex surrounded by a wide moat and intricate carvings.",
    lat: 13.4124693,
    lng: 103.8669857,
  },

  // Canada
  {
    name: "Peyto Lake",
    description:
      "Unique glacier-fed waterway in a valley known for its bright turquoise water & scenic beauty.",
    lat: 51.77835376750945,
    lng: -116.53237765205041,
  },

  // Chile
  {
    name: "Torres del Paine National Park",
    description: "National Park in Patagonia.",
    lat: -50.8604868,
    lng: -73.284805,
  },

  // China
  {
    name: "Harbin Ice Festival",
    description:
      "Ice Festival featuring giant colorful sculptures and buildings made of ice.",
    lat: 45.777671,
    lng: 126.6207933,
    img_name: "harbin.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/50788960847",
    visited: true,
  },
  {
    name: "Longmen Grottoes",
    description: "Vast, UNESCO–listed site with thousands of statues of Buddha carved in caves excavated from cliffs.",
    lat: 34.5983596843618,
    lng: 112.47349736240065,
  },
  {
    name: "Mogao Caves",
    description: "Complex featuring 492 cave temples with Buddhist artwork, believed to span from 4th-14th centuries.",
    lat: 40.11597653917494,
    lng: 94.83222718539928,
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
  },
  {
    name: "Classical Gardens of Suzhou",
    description: "Built over 1000 years, from the 11th-19th centuries, these gardens define the Chinese style.",
    lat: 31.33455547912024,
    lng: 120.62878325062533,
  },
  {
    name: "Shilin Stone Forest in Yunnan",
    description: "Rocky, karst parkland featuring a forest of tall limestone formations & green trees.",
    lat: 24.82363552976476,
    lng: 103.32756741435762,
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
  },

  // Croatia
  {
    name: "Plitvice Lakes National Park",
    description:
      "It's known for a chain of 16 terraced lakes, joined by waterfalls, that extend into a limestone canyon.",
    lat: 44.8653966,
    lng: 15.5820119,
  },

  // Egypt
  {
    name: "Giza Necropolis",
    description: "Includes the Pyramid of Giza, the only remaining of the Seven Wonders of the Ancient World.",
    lat: 29.97747274226785,
    lng:  31.13245258817376,
    img_name: "giza.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/51226844608",
    visited: true,
  },
  {
    name: "Luxor Temple",
    description: "Ancient Egyptian temple complex.",
    lat: 25.699502,
    lng: 32.6390509,
    img_name: "luxor.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/50787740683",
    visited: true,
  },

  // England
  {
    name: "British Museum",
    description: "One of the largest natural history museums in the world.",
    lat: 51.5193839182863,
    lng: -0.1269542555425667,
    img_name: "rosetta.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/51334653908",
    visited: true,
  },
  {
    name: "White Cliffs of Dover",
    description: "White chalk cliffs rise 350 feet from the sea.",
    lat: 51.13473906458341,
    lng: 1.3572182590023552,
    img_name: "dover_cliffs.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/51333705027",
    visited: true,
  },
  // Equador
  {
    name: "Galapagos Island",
    description: "Darwin.",
    lat: -0.82927,
    lng: -90.98206,
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
  },

  // France
  {
    name: "Lavender Fields of Provence",
    description: "Lavender Fields of Provence.",
    lat: 43.86508366667945,
    lng: 5.427702093675174,
  },

  // Germany
  {
    name: "Neuschwanstein Castle",
    description: "Turreted, 19th-century, hilltop castle built for King Ludwig II.",
    lat: 47.55815322752,
    lng: 10.749821857890307,
  },

  // Greece
  {
    name: "Monasteries of Meteora",
    description:
      "Monasteries built in the 12-13th century on top of tall rock pillars",
    lat: 39.7217044,
    lng: 21.6305896,
    img_name: "meteora.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/50788014528/",
    visited: true,
  },
  {
    name: "Santorini",
    description: "Small Greek Island known for its while buildings",
    lat: 36.4624199,
    lng: 25.3789717,
    img_name: "santorini.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/50788014863/",
    visited: true,
  },

  // Guatemala
  {
    name: "Tikal",
    description: "Tikal is an ancient Mayan citadel.",
    lat: 17.22489,
    lng: -89.61103,
  },

  // Iceland
  {
    name: "Svartifoss",
    description: "Waterfall and basalt rocks in Skaftafell National Park.",
    lat: 64.0234109,
    lng: -16.9841541,
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
    lat: 27.007379920230612,
    lng: 76.60643437687226,
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
    img_link: "https://www.flickr.com/photos/kunigami/50787720548",
    visited: true,
  },
  {
    name: "Taj Mahal",
    description: "Beautiful palace of Mughal architecture in Agra, India.",
    lat: 27.1737721,
    lng: 78.0427502,
    img_name: "taj_mahal.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/50788945632",
    visited: true,
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


  // Indonesia
  {
    name: "Borobudur",
    description:
      "Dating to the 9th century, this site is renowned as the largest Buddhist temple in the world.",
    lat: -7.607342627039068,
    lng: 110.2037284446244,
  },

  // Iran
  {
    name: "Bam Citadel",
    description: "The Arg-e Bam is the largest adobe building in the world.",
    lat: 29.115176273815155,
    lng: 58.369317332187286,
  },
  {
    name: "Persepolis",
    description: "Ruins of a 518 BC Achaemenid empire capital.",
    lat: 29.933267569582167,
    lng: 52.886444738853385,
  },
  {
    name: "Shushtar Historical Hydraulic System",
    description: "Complex irrigation system & UNESCO site with water mills, dams, tunnels & canals.",
    lat: 32.044218764946585,
    lng: 48.85846733320769,
  },

  // Iraq
  {
    name: "Ziggurat of Ur",
    description: "Monumental place of worship built for a Bronze Age Sumerian king, often restored over 4,000 years.",
    lat: 30.96649516884495,
    lng: 46.10404623212625,
  },

  // Ireland
  {
    name: "Cliffs of Moher",
    description: "Cliffs in Ireland.",
    lat: 52.97278178463433,
    lng: -9.430060029427306,
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
    name: "Colosseum",
    description:
      "Largest ancient amphitheatre ever built, and is still the largest standing amphitheater in the world today.",
    lat: 41.8902102,
    lng: 12.4900422,
  },
  {
    name: "Venice",
    description:
      "Venice, the capital of northern Italy’s Veneto region, is built on more than 100 small islands in a lagoon in the Adriatic Sea. It has no roads, just canals",
    lat: 45.43440028570622,
    lng: 12.338469167604442,
  },

  // Japan
  {
    name: "Fushimi Inari Shrine",
    description: "Known for its numerous Torii gates",
    lat: 34.967340614218074,
    lng: 135.77263945495233,
    img_name: "fushimi_inari.jpg",
    photographer: "me",
    img_link: "https://www.flickr.com/photos/kunigami/50807076678/",
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
    img_link: "https://www.flickr.com/photos/kunigami/50788808272/",
    visited: true,
  },
  {
    name: "Ashikaga Flower Park",
    description:
      "Charming flower gardens offering 8 seasonal thematic displays & a spring wisteria festival.",
    lat: 36.315072480873724,
    lng: 139.52010803165786,
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
    img_link: "https://www.flickr.com/photos/kunigami/50787982268",
    visited: true,
  },

  // Madagascar
  {
    name: "Avenue of the Baobabs",
    description:
      "Famed group of towering baobab trees lining the dirt road from Morondava to Belon'i Tsiribihina.",
    lat: -20.2560794,
    lng: 44.1756571,
  },
  {
    name: "Tsingy de Bemaraha Strict Nature Reserve",
    description:
      "A UNESCO World Heritage Site, this park is known for its dramatic limestone geological formations.",
    lat: -18.4368781,
    lng: 44.7381432,
  },

  // Malaysia
  {
    name: "Batu Caves",
    description:
      "Limestone caves at the top of steep steps housing Hindu temples & shrines, plus a huge deity statue.",
    lat: 3.2602148,
    lng: 101.5868289,
  },

  // Mexico
  {
    name: "Gran Cenote",
    description:
      "One of multiple cenotes (underground sinkholes) in the Yucatan Peninsula.",
    lat: 20.246823368379435,
    lng: -87.46415808600378,
  },
  {
    name: "Chichén-Itzá",
    description:
      "Archaeological site with excavated ruins of the large Maya city.",
    lat: 20.680334,
    lng: -88.5707365,
  },
  {
    name: "Hierve el Agua",
    description:
      "Ancient geological site featuring towering, waterfall-like rock formations, pools & springs.",
    lat: 16.865753242292385,
    lng: -96.27599567087927,
  },
  {
    name: "Palenque National Park",
    description:
      "Palenque, also anciently known as Lakamha, was a Maya city state in southern Mexico that flourished in the 7th century.",
    lat: 17.485512166526405,
    lng: -92.04572560297886,
  },

  // Myanmar
  {
    name: "Old Bagan",
    description:
      "Bagan is an ancient city and a UNESCO World Heritage Site in the Mandalay Region of Myanmar",
    lat: 21.171739824825504,
    lng: 94.85881533957875,
  },

  // Morocco
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
  },

  // Nepal
  {
    name: "Mount Everest Base Camp",
    description: "n/a",
    lat: 27.9343193,
    lng: 86.7818523,
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
    name: "Piopiotahi",
    description:
      "Milford Sound is a fiord in the southwest of New Zealand’s South Island. It’s known for towering Mitre Peak, plus rainforests and waterfalls like Stirling and Bowen falls, which plummet down its sheer sides.",
    lat: -44.63507813222853,
    lng: 167.89796460430344,
  },

  // Norway
  {
    name: "Geirangerfjord",
    description:
      "Iconic fjord offering snowy mountain peaks, lush plant life, cascading falls & spectacular views.",
    lat: 62.10331827445209,
    lng: 7.095297294161887,
  },
  {
    name: "Preikestolen",
    description:
      "Popular mountainous hike to a famed 604-metre cliff with a flat top, offering panoramic views.",
    lat: 58.9868234,
    lng: 6.186655,
  },

  // Peru
  {
    name: "Machu Picchu",
    description: "Ancient Inca Site.",
    lat: -13.1631412,
    lng: -72.5471516,
  },
  {
    name: "Titicaca",
    description: "Largest lake in South America between Bolivia and Peru.",
    lat: -15.4885423,
    lng: -69.3133206,
  },

  // Philippines
  {
    name: "Chocolate Hills Complex",
    description:
      "Popular observation area known for its panoramic views of its hilly, tree-filled surrounds.",
    lat: 9.80489371215848,
    lng: 124.16906501364669,
  },

  // Scotland
  {
    name: "The Storr",
    description: "Rock formation in Northern Scotland.",
    lat: 57.4959496,
    lng: -6.1972455,
  },

  // Spain
  {
    name: "Ronda",
    description: "Beautiful city atop of a mountain.",
    lat: 36.7462,
    lng: -5.16122,
  },
  {
    name: "Mosque–Cathedral of Córdoba",
    description:
      "The Mosque–Cathedral of Córdoba, officially known by its ecclesiastical name, the Cathedral of Our Lady of the Assumption is the cathedral of the Roman Catholic Diocese of Córdoba dedicated to the Assumption of Mary and located in the Spanish region of Andalusia.",
    lat: 37.8789,
    lng: -4.77938,
  },

  // Sweden
  {
    name: "Kungsleden",
    description:
      "Kungsleden is a hiking trail in northern Sweden, approximately 440 kilometres long, between Abisko in the north and Hemavan in the south. It passes through, near the southern end, the Vindelfjällen Nature Reserve, one of the largest protected areas in Europe.",
    lat: 68.36168,
    lng: 18.7234,
  },

  // Tanzania
  {
    name: "Serengeti National Park",
    description:
      "Vast nature reserve best known for its annual wildebeest migration, with lions, elephants & rhino.",
    lat: -2.333880200584234,
    lng: 34.83305583000156,
  },

  // Turkey
  {
    name: "Cappadocia",
    description:
      "Known for its distinctive “fairy chimneys,” tall, cone-shaped rock formations clustered in Monks Valley, Göreme and elsewhere",
    lat: 38.649597784097764,
    lng: 34.83383186504822,
  },
  {
    name: "Pamukkale",
    description: "Cotton Castle (Turkish). Thermal Springs",
    lat: 37.92337084547643,
    lng: 29.131354362511228,
  },

  // Turkmenistan
  {
    name: "Darvaza Gas Crater",
    description:
      "Natural gas field with a collapsed crater thought to have been burning continuously since 1971.",
    lat: 40.2526031,
    lng: 58.4397004,
  },

  // UAE
  {
    name: "Sheikh Zayed Grand Mosque",
    description: "The largest mosque in Abu Dhabi.",
    lat: 24.423573631874092,
    lng: 54.47371836430741,
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
  },

  // Vietnam
  {
    name: "Hạ Long Bay",
    description: "Multiple tree-covered limestone islands.",
    lat: 20.9361865,
    lng: 107.1593762,
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
    name: "Socotra Island",
    description: "Island with the unique dragon's blood tree.",
    lat: 12.511573363989033,
    lng: 53.83185149889994,
  },
];
