
/**
 * deepClone
 * @param data Raw data of any type
 * @returns Data after deepClone
 */
export function deepClone(data: any) {
  let res: any = null;
  const reference = [Date, RegExp, Set, WeakSet, Map, WeakMap, Error];
  if (reference.includes(data?.constructor)) {
    res = new data.constructor(data);
  } else if (Array.isArray(data)) {
    res = [];
    data.forEach((e, i) => {
      res[i] = deepClone(e);
    });
  } else if (typeof data === 'object' && data !== null) {
    res = {};
    Object.keys(data).forEach((key) => {
      if (Object.hasOwnProperty.call(data, key)) {
        res[key] = deepClone(data[key]);
      }
    });
  } else {
    res = data;
  }
  return res;
}

/**
 * Get the value of the specified key from window.location.href
 * @param {*} key The key to get
 * @returns The value corresponding to the key specified in window.location.href.
 * @example
 * const value = getUrlParam(key);
 */
export function getUrlParam(key: string) {
  const url = window?.location.href.replace(/^[^?]*\?/, '');
  const regexp = new RegExp(`(^|&)${key}=([^&#]*)(&|$|)`, 'i');
  const paramMatch = url?.match(regexp);

  return paramMatch ? paramMatch[2] : null;
}

export function getUrlParams(): Record<string, string> {
  const query: Record<string, string> = {};
  const hashQueryIndex = location.href.indexOf('?');
  if (hashQueryIndex !== -1) {
    const hashQueryString = location.href.substring(hashQueryIndex + 1);
    const params = new URLSearchParams(hashQueryString);

    params.forEach((value, key) => {
      query[key] = value;
    });
  }
  return query;
};
