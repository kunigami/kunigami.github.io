const PUBLIC_TOKEN =
  "pk.eyJ1Ijoia3VuaWdhbWkiLCJhIjoiY2tqNmVwYmZwMWl3YzJzcGtmNzMyZ2hkbSJ9.Jm_dwc5XmTDCYsWJ3BZnfg";
const ATTRIBUTION =
  'Map data &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, Imagery © <a href="https://www.mapbox.com/">Mapbox</a>';

const green_icon = new L.Icon({
  iconUrl:
    "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
  shadowUrl:
    "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
  iconSize: [25, 41],
  iconAnchor: [12, 41],
  popupAnchor: [1, -34],
  shadowSize: [41, 41],
});

class Wanderlust extends React.Component {
  componentDidMount() {
    renderMap();
  }

  render() {
    const total_markers = markers.length;
    // TODO: make this more responsive on the width
    return (
      <div style={{ margin: 50 }}>
        <h1>Wanderlust</h1>

        <p>
          The world is full of amazing places to visit. In this page I share
          some of the spots I'd like to visit one day or that I've visited
          already (green marker). There are currently {total_markers} places in
          the map.
        </p>

        <p>
          You'll note this map is heavily skewed towards Western USA (since that's where
          I have more opportunities to explore) and natural places (personal preference). If
          you have other places on your wanderlust list, please let me know!
        </p>

        <p>NOTE: This page is better viewed on desktop.</p>

        <div>
          <div id="mapid" style={{ height: 1200 }}></div>
        </div>
      </div>
    );
  }
}

ReactDOM.render(<Wanderlust />, document.querySelector("#app"));

function renderMap() {
  const leftlet_map = L.map("mapid").setView([0, 10], 3);

  L.tileLayer(
    `https://api.mapbox.com/styles/v1/{id}/tiles/{z}/{x}/{y}?access_token=${PUBLIC_TOKEN}`,
    {
      attribution: ATTRIBUTION,
      maxZoom: 18,
      id: "mapbox/streets-v11",
      tileSize: 512,
      zoomOffset: -1,
      accessToken: PUBLIC_TOKEN,
    }
  ).addTo(leftlet_map);

  const marker_cluster = L.markerClusterGroup();
  for (let i = 0; i < markers.length; ++i) {
    const marker_data = markers[i];
    const popup_renderer = () => {
      const popop_div_root = document.createElement("div");
      ReactDOM.render(
        <Popup
          name={marker_data.name}
          description={marker_data.description}
          photographer={marker_data.photographer}
          img_name={marker_data.img_name}
          img_link={marker_data.img_link}
          lat={marker_data.lat}
          lng={marker_data.lng}
        />,
        popop_div_root
      );
      return popop_div_root;
    };

    let opt = {};
    if (marker_data.visited) {
      opt = { icon: green_icon };
    }

    const marker = L.marker(
      [marker_data.lat, marker_data.lng],
      opt
    ).bindPopup(popup_renderer, { minWidth: 320 });

    marker_cluster.addLayer(marker);
  }

  leftlet_map.addLayer(marker_cluster);
}

function GoogleMapsLink({ lat, lng, children }) {
  return (
    <a href={`https://www.google.com/maps/place/${lat},${lng}`} target="_blank">
      {children}
    </a>
  );
}

function Popup({
  name,
  description,
  img_name,
  img_link,
  photographer,
  lat,
  lng,
}) {
  let img_src = null;
  if (img_name) {
    img_src = `../resources/wanderlust/${img_name}`;
  }

  let img_dom = null;
  if (img_src) {
    photographer = photographer
      ? photographer == "me"
        ? "Guilherme Kunigami"
        : photographer
      : "Unknown";
    if (img_link) {
      photographer = (
        <a href={img_link} target="_blank">
          {photographer}
        </a>
      );
    }
    const attribution = <span>Photo by: {photographer}</span>;
    img_dom = (
      <figure>
        <GoogleMapsLink lat={lat} lng={lng}>
          <img src={img_src} />
        </GoogleMapsLink>
        <figcaption>{attribution}</figcaption>
      </figure>
    );
  }

  return (
    <div
      style={{ alignItems: "center", display: "flex", flexDirection: "column" }}
    >
      <p>
        <b>{name}</b>
      </p>
      <p>
        {description ? description + " " : ""}
        <GoogleMapsLink lat={lat} lng={lng}>
          (Google maps)
        </GoogleMapsLink>
      </p>
      {img_dom}
    </div>
  );
}
