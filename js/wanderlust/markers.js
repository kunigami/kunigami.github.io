// General notes:
//   - Resize images to be 300px on the largest dimension
const markers = [
  // United States
  // -- Arizona
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
  // -- California
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

  // Egypt
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

  // Japan
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

  // India
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

  // Norway
  {
    name: "Preikestolen",
    description:
      "Popular mountainous hike to a famed 604-metre cliff with a flat top, offering panoramic views.",
    lat: 58.9868234,
    lng: 6.186655,
  },
];
